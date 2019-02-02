----------------------------------------------------------------------------
--
--  Atmel AVR Register Array Test Entity Declaration
--
--  This is the entity declaration which must be used for building the
--  register array portion of the AVR design for testing.
--
--  Revision History:
--     17 Apr 98  Glen George       Initial revision.
--     20 Apr 98  Glen George       Fixed minor syntax bugs.
--     22 Apr 02  Glen George       Updated comments.
--     18 Apr 04  Glen George       Updated comments and formatting.
--     21 Jan 06  Glen George       Updated comments.
--     31 Jan 19  Sundar Pandian    Added support for CPU testing
--
----------------------------------------------------------------------------


--
--  REG_TEST
--
--  This is the register array testing interface.  It just brings all the
--  important register array signals out for testing along with the
--  Instruction Register.
--
--  Inputs:
--    IR      - Instruction Register (16 bits)
--    RegIn   - input to the register array (8 bits)
--    clock   - the system clock
--
--  Outputs:
--    RegAOut - register bus A output (8 bits), eventually will connect to ALU
--    RegBOut - register bus B output (8 bits), eventually will connect to ALU
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

use work.opcodes.all;


entity  REG_TEST  is

    port(
        IR       :  in  opcode_word;                        -- Instruction Register
        RegIn    :  in  std_logic_vector(7 downto 0);       -- input register bus
        clock    :  in  std_logic;                          -- system clock
        RegAOut  :  out std_logic_vector(7 downto 0);       -- register bus A out
        RegBOut  :  out std_logic_vector(7 downto 0)        -- register bus B out
    );

end  REG_TEST;

architecture regteste_arc of reg_test is

	signal SReg : std_logic_vector(7 downto 0);

    signal    RegWEn      : std_logic; -- register write enable
    signal    RegWSel     : std_logic_vector(4 downto 0); -- register write select
    signal    RegSelA     : std_logic_vector(4 downto 0); -- register A select
    signal    RegSelB     : std_logic_vector(4 downto 0); -- register B select
    signal    LoadIn      : std_logic_vector(1 downto 0);
    signal    LoadReg     : std_logic_vector(1 downto 0);

        -- to ALU and SReg
    signal    ALUOp   : std_logic_vector(3 downto 0); -- operation control signals
    signal    ALUSel  : std_logic_vector(2 downto 0); -- operation select
    signal    bitmask : std_logic_vector(7 downto 0); -- mask for writing to flags

        -- I/O
    signal    IORegInEn   : std_logic;                      --
    signal    IORegOutEn  : std_logic;                      --
    signal    SRegOut     : std_logic_vector(7 downto 0);
    signal    SRegLd      : std_logic;
    signal    K           : std_logic_vector(7 downto 0); -- immediate value K

    signal 	  data_bus_in : std_logic_vector(7 downto 0);
    signal 	  IOdata 	  : std_logic_vector(7 downto 0);
    signal 	  ALUdata     : std_logic_vector(7 downto 0);

    signal    IOAddr 	  : std_logic_vector(5 downto 0);

begin

    CtrlU   : entity work.CU port map(IR, SReg, RegWEn, RegWSel, RegSelA,
                                   RegSelB, LoadIn, LoadReg, ALUOp, ALUSel,
                                   bitmask, IORegInEn, IORegOutEn, SRegOut, SRegLd,
                                   K, clock);

    --data_muxer: for i in 7 downto 0 generate
    --data_line: entity work.Mux
    --	port map (
    --		LoadReg(0), LoadReg(1), K(i), ALUdata(i), IOdata(i), RegAOut(i),
    --		data_bus_in(i)
    --	);
    --end generate data_muxer;

    -- 2 to 1 mux as well for muxing in SReg/bitmask lines from both CtrlU and ALU

    IOAddr <= K(6 downto 5)&K(3 downto 0);

    IORegSpace : entity work.IOReg port map(RegIn, IOAddr,
								SRegOut, clock, IORegInEn, IORegOutEn, bitmask, IOdata,
								SReg);

    RegSpace : entity work.Reg port map(RegIn, clock,  RegWEn, RegWSel, RegSelA,
                                   RegSelB, RegAOut, RegAOut, LoadReg);


end regteste_arc;

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