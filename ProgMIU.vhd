----------------------------------------------------------------------------
--
--
-- Program Memory Interface Unit
--
-- The program memory access unit generates the addresses for reading the
-- program memory data. The program memory is addressed as 16-bit words
-- with 16-bit addresses. The program counter, containing the currently
-- executing instruction, is located inside this unit and is incremented
-- or loaded as necessary for the next address.
--
--  Inputs:
--
--  reset   - active low reset signal
--  clock   - system clock
--  Load    - 1 bit load select for PC, from CU
--  AddrSourceSel   - 3 bit address source select, from CU
--  IR_input        - 16 bit instruction reg input
--  Z_input         - 16 bit address from Z
--  DataDB          - 8 bit data data bus
--
--  Outputs:
--
--  ProgAB          - 16 bit program address bus
--
-- Revision History:
-- 01/24/2019 Sophia Liu Initial revision
--
----------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

use work.opcodes.all;
use work.constants.all;
use work.ALUconstants.all;

entity ProgMIU is
    port(
	    reset	: in std_logic;         -- active low reset signal
        clock   : in std_logic;         -- system clock
        Load    : in std_logic;         -- load select for PC, from CU
        AddrSourceSel : in SOURCE_SEL;  -- address source select, from CU
        IR_input: in std_logic_vector(ADDRSIZE-1 downto 0); -- instruction reg

        -- from RegUnit
        Z_input : in std_logic_vector(ADDRSIZE-1 downto 0); -- z addr, for ICALL
        DataDB  : in std_logic_vector(REGSIZE-1 downto 0);  -- data data bus
        ProgAB  : out std_logic_vector(ADDRSIZE-1 downto 0) -- program address bus
     );
end ProgMIU;

architecture ProgMIU_arc of ProgMIU is

component fullAdder is
    port(
        A           :  in      std_logic;  -- adder input
        B           :  in      std_logic;  -- adder input
        Cin         :  in      std_logic;  -- carry in value
        Cout        :  out     std_logic;  -- carry out value
        Sum         :  out     std_logic   -- sum of A, B with carry in
      );
end component;

signal OffsetMuxOut : std_logic_vector(ADDRSIZE-1 downto 0);

signal ProgCtr      : std_logic_vector(ADDRSIZE-1 downto 0);

signal PCOut        : std_logic_vector(ADDRSIZE-1 downto 0);

signal CarryOut     : std_logic_vector(ADDRSIZE-1 downto 0);    -- carry for adder/subtracter

signal AddrAdderOut : std_logic_vector(ADDRSIZE-1 downto 0); -- address adder output

begin

    OffsetMuxOut <= IR_input            when AddrSourceSel = IR_SRC else
                    Z_input             when AddrSourceSel = Z_SRC else
                    RST_VECTOR          when AddrSourceSel = RST_SRC else
                    INC_VECTOR          when AddrSourceSel = NORMAL_SRC else
                    "00000000" & DataDB when AddrSourceSel = DB_LO_SRC else
                    DataDB & "00000000" when AddrSourceSel = DB_HI_SRC;

    AndBlock: for i in ADDRSIZE-1 downto 0 generate
        PCOut(i) <= ProgCtr(i) and Load;
    end generate AndBlock;

    -- ADDRSIZE bit adder for indirect address manipulations
    adder0: fullAdder
    port map(
        A           => PCOut(0),         -- indirect address to manipulate
        B           => OffsetMuxOut(0),     -- only ever adding in offset values
        Cin         => '0',                 -- since only adding in offset, carry always '0'
        Cout        => Carryout(0),         -- set next carry
        Sum         => AddrAdderOut(0)      -- save to address adder buffer
    );
    -- other bits
    GenAdder:  for i in 1 to ADDRSIZE - 1 generate
    adderi: fullAdder
    port map(
        A           => PCOut(i),         -- indirect address to manipulate
        B           => OffsetMuxOut(i),     -- only ever adding in offset values
        Cin         => CarryOut(i-1),       -- carry in from last bit add
        Cout        => Carryout(i),         -- set next carry
        Sum         => AddrAdderOut(i)      -- save to address adder buffer
    );
    end generate GenAdder;

    ProgAB <= AddrAdderOut;

    process(clock)
    begin
        if rising_edge(clock) then
            ProgCtr <= AddrAdderOut;
        end if;
        if rising_edge(clock) and reset = '0' then
            ProgCtr <= "1111111111111111";--(others => '1');
        end if;
    end process;

end ProgMIU_arc ; -- ProgMIU_arc
