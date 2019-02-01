----------------------------------------------------------------------------
-- 
-- 
-- Arithmetic Logic Unit  
--
-- ALU implementation for the AVR CPU, responsible for arithmetic and logic
-- operations, including boolean operations, shifts and rotates, bit functions,
-- addition, subtraction, and comparison. The operands may be registers or 
-- immediate values. 
-- The ALU consists of a functional block for logical operations, 
-- an adder/subtracter, and a shifter/rotator. It takes several control 
-- signals from the control unit as inputs, along with the operands A 
-- and B from the register array, or from an immediate value encoded in the 
-- instruction. It outputs the computed result and computed flags.
--
-- Ports:      
--  Inputs:  
--        ALUOp   - 4 bit operation control signal 
--        ALUSel  - 2 bit control signal for the final operation select 
--        RegA    - 8 bit operand A 
--        RegB    - 8 bit operand B         
--
--  Outputs:
--        RegOut  - 8 bit output result        
--        StatusOut - 8 bit status flags to status register
--
-- Revision History:
-- 01/24/2019   Sophia Liu      Initial revision
-- 01/28/2019   Sophia Liu      Initial architecture revision
-- 01/31/2019   Sundar Pandian  Added support for BST, BLD
--
----------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

library opcodes; 
use work.opcodes.all; 

use work.ALUconstants.all; 

entity ALU is
    port(
        --Clk     : in std_logic; -- system clock
        -- from CU
        ALUOp   : in std_logic_vector(3 downto 0); -- operation control signals 
        ALUSel  : in std_logic_vector(1 downto 0); -- operation select 
        
        -- from Regs 
        RegA    : in std_logic_vector(REGSIZE-1 downto 0); -- operand A
        RegB    : in std_logic_vector(REGSIZE-1 downto 0); -- operand B, or immediate 
        
        FlagMask: out std_logic_vector(REGSIZE - 1 downto 0); -- mask for writing to status flags
        RegOut  : out std_logic_vector(REGSIZE-1 downto 0); -- output result
        StatusOut    : out std_logic_vector(REGSIZE-1 downto 0) -- status register output
    );
end ALU;

architecture behavioral of ALU is 

-- internal signals
signal AdderOut : std_logic_vector(REGSIZE-1 downto 0); -- adder/subtracter output
signal CarryOut: std_logic_vector(REGSIZE-1 downto 0); -- carry for adder/subtracter

signal Fout : std_logic_vector(REGSIZE-1 downto 0); -- f block output 

signal SRout : std_logic_vector(REGSIZE-1 downto 0); -- shifter/rotator block output 

signal Bout : std_logic_vector(REGSIZE-1 downto 0); -- bit set block output 

signal Status : std_logic_vector(REGSIZE-1 downto 0); -- sreg buffer

signal RegBuff  : std_logic_vector(REGSIZE-1 downto 0); -- buffer for output result ?

-- internal status signals 
signal NFlag : std_logic; -- negative status flag
signal CFlag : std_logic; -- carry status flag
signal VFlag : std_logic; -- signed overflow status flag 
signal TFlag : std_logic; -- transfer flag

-- component declarations 
component fullAdder is
    port(
        A           :  in      std_logic;  -- adder input 
        B           :  in      std_logic;  -- adder input 
        Subtract    :  in      std_logic;  -- active for subtracting
        Cin         :  in      std_logic;  -- carry in value 
        Cout        :  out     std_logic;  -- carry out value 
        Sum         :  out     std_logic   -- sum of A, B with carry in
      );
end component;

component Mux is
    port(
        S0          :  in      std_logic;  -- mux sel (0) 
        S1          :  in      std_logic;  -- mux sel(1) 
        SIn0         :  in      std_logic;  -- mux inputs
        SIn1         :  in      std_logic;  -- mux inputs
        SIn2         :  in      std_logic;  -- mux inputs
        SIn3         :  in      std_logic;  -- mux inputs
        SOut        :  out     std_logic   -- mux output
      );
end component;  

begin

    -- adder/subtracter 
    -- low bit, with carry in
    adder0: fullAdder
        port map(
            A           => RegA(0),
            B           => RegB(0),
            Subtract    => ALUOp(0),
            Cin         => ALUOp(1),
            Cout        => Carryout(0), 
            Sum         => AdderOut(0)
      );
      -- other bits 
      GenAdder:  for i in 1 to REGSIZE - 1 generate
      adderi: fullAdder
        port map(
            A           => RegA(i),
            B           => RegB(i),
            Subtract    => ALUOp(0),
            Cin         => CarryOut(i-1), --?
            Cout        => Carryout(i), 
            Sum         => AdderOut(i)
      );
      end generate GenAdder;
      
    -- fblocks 
    GenFBlock:  for i in REGSIZE-1 downto 0 generate
      FBlocki: Mux
        port map(
            S0          => RegA(i),
            S1          => RegB(i),
            SIn0        => ALUOp(0),
            SIn1        => ALUOp(1),
            SIn2        => ALUOp(2),
            SIn3        => ALUOp(3),
            SOut        => FOut(i)
      );
      end generate GenFBlock;
      
    -- shifter/rotator
    -- assign middle and low bits 
    SROut(REGSIZE - 2 downto 0) <= RegA(REGSIZE - 1 downto 1); 
    -- assign high bit 
    SRMux: Mux
        port map(
            S0          => ALUOp(0),
            S1          => ALUOp(1),
            SIn0        => '0',             -- LSR high bit = 0
            SIn1        => RegA(REGSIZE-1), -- ASR high bit constant
            SIn2        => ALUOp(2),        -- ROR high bit = carry in 
            SIn3        => 'X',
            SOut        => SROut(REGSIZE-1)
      );
      
    -- end mux 

    -- transfer bit loading
    BIT_OP : for i in 0 to REGSIZE-1 generate
        Bout(i) <= ALUOp(0) when i = conv_integer(RegB) else
                   RegA(i);
    end generate; --??

    GenALUSel:  for i in REGSIZE-1 downto 0 generate
    ALUSelMux: Mux
        port map(
            S0          => ALUSel(0),
            S1          => ALUSel(1),
            SIn0        => AdderOut(i),
            SIn1        => FOut(i), 
            SIn2        => SROut(i),
            SIn3        => Bout(i),			
            SOut        => RegBuff(i)
      );
      end generate GenALUSel;
    
    -- Status Register logic
    
    -- transfer, interrupt bits not set through ALU
    --StatusOut(7) <= 'X'; 
    -- transfer bit
    StatusOut(6) <= RegA(conv_integer(RegB));
    
    -- half carry 
    StatusOut(5) <= CarryOut(HALFCARRYBIT);
	 
	 -- corrected signed 
    StatusOut(4) <= RegBuff(REGSIZE-1) xor Status(3); -- N xor V 
    
    -- signed overflow 
    StatusOut(3) <= '1' when (ALUSEL = ADDSUBEN and CarryOut(REGSIZE-1)= CarryOut(REGSIZE-2)) else -- 1 if signed overflow  
                 '0' when (ALUSEL = ADDSUBEN or  ALUSEL = FBLOCKEN) else -- 0 if no overflow or logical op
                 (RegBuff(REGSIZE-1) xor RegA(0)) when ALUSEL = SHIFTEN; --N xor C for shift operations 
						  
    -- negative
    StatusOut(2) <= RegBuff(REGSIZE-1); 
    
    -- zero flag 
    StatusOut(1) <= '1' when RegBuff = ZERO8 else 
                    '0';
    -- carry
    StatusOut(0) <= CarryOut(REGSIZE-1) when ALUSel = ADDSUBEN else 
                   RegA(0) when ALUSel = SHIFTEN; 
						 
    
    

end behavioral;  


----------------------------------------------------------------------------
--
--  1 Bit Full Adder
--
--  Implementation of a full adder/subtracter. This entity takes the one bit 
--  inputs A and B with a carry in input and outputs the sum/diff and carry 
--  out bits, using combinational logic. 
--
-- Inputs:
--      A: std_logic - 1 bit adder input
--      B: std_logic - 1 bit adder input
--      Subtract: std_logic - 1 bit subtract input 
--      Cin: std_logic - 1 bit carry in input
--
-- Outputs:
--      Sum: std_logic - 1 bit sum of A, B, and Cin
--      Cout: std_logic - 1 bit carry out value 
--
--  Revision History:
--      11/21/18  Sophia Liu    Initial revision.
--      11/22/18  Sophia Liu    Updated comments.
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity fullAdder is
    port(
        A           :  in      std_logic;  -- adder input 
        B           :  in      std_logic;  -- adder input 
        Subtract    :  in      std_logic;  -- active for subtracting
        Cin         :  in      std_logic;  -- carry in value 
        Cout        :  out     std_logic;  -- carry out value 
        Sum         :  out     std_logic   -- sum of A, B with carry in
      );
end fullAdder;

architecture fullAdder of fullAdder is
    begin
        -- combinational logic for calculating the sum and carry out bit
        Sum <= A xor B xor Cin xor Subtract;
        Cout <= ((A xor Subtract) and B) or ((A xor Subtract) and Cin) or (B and Cin);
end fullAdder;



--------------------------- 4:1 mux

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Mux is
    port(
        S0          :  in      std_logic;  -- mux sel (0) 
        S1          :  in      std_logic;  -- mux sel(1) 
        SIn0         :  in      std_logic;  -- mux inputs
        SIn1         :  in      std_logic;  -- mux inputs
        SIn2         :  in      std_logic;  -- mux inputs
        SIn3         :  in      std_logic;  -- mux inputs
        SOut        :  out     std_logic   -- mux output
      );
end Mux;

architecture Mux of Mux is
    begin
    process(SIn0, SIn1, SIn2, SIn3, S0, S1)
    begin  
        if S0 = '0' and S1 = '0' then 
            SOut <= SIn0; 
        elsif S0 = '0' and S1 = '1' then 
            SOut <= SIn1; 
        elsif S0 = '1' and S1 = '0' then 
            SOut <= SIn2; 
        elsif S0 = '1' and S1 = '1' then 
            SOut <= SIn3; 
        else 
            SOut <= 'X'; -- for sim  
        end if;   
    end process;
end Mux;
