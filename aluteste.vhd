----------------------------------------------------------------------------
--
--  Atmel AVR ALU Test Entity Declaration
--
--  This is the entity declaration which must be used for building the ALU
--  portion of the AVR design for testing.
--
--  Revision History:
--     17 Apr 98  Glen George       Initial revision.
--     20 Apr 98  Glen George       Fixed minor syntax bugs.
--     18 Apr 04  Glen George       Updated comments and formatting.
--     21 Jan 06  Glen George       Updated comments.
--		 31 Jan 19  Sophia Liu        Updated architecture
--
----------------------------------------------------------------------------


--
--  ALU_TEST
--
--  This is the ALU testing interface.  It just brings all the important
--  ALU signals out for testing along with the Instruction Register.
--
--  Inputs:
--    IR       - Instruction Register (16 bits)
--    OperandA - first operand to ALU (8 bits) - looks like the output
--               of the register array
--    OperandB - second operand to ALU (8 bits) - looks like the output
--               of the register array
--    clock    - the system clock
--
--  Outputs:
--    Result   - result of the ALU operation selected by the Instruction
--               Register (8 bits)
--    StatReg  - Status Register contents (8 bits)
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

library opcodes;
use opcodes.opcodes.all;

use work.ALUConstants.all;

entity  ALU_TEST  is

    port(
        IR        :  in  opcode_word;                       -- Instruction Register
        OperandA  :  in  std_logic_vector(7 downto 0);      -- first operand
        OperandB  :  in  std_logic_vector(7 downto 0);      -- second operand
        clock     :  in  std_logic;                         -- system clock
        Result    :  out std_logic_vector(7 downto 0);      -- ALU result
        StatReg   :  out std_logic_vector(7 downto 0)       -- status register
    );

end  ALU_TEST;

architecture ALUTB of ALU_TEST is 
				signal ALUOp   : std_logic_vector(3 downto 0); -- operation control signals 
            signal ALUSel  : std_logic_vector(2 downto 0); -- operation select 
            
				signal FlagMask: std_logic_vector(REGSIZE - 1 downto 0); -- mask for writing to status flags
            signal ALUStatusOut    : std_logic_vector(REGSIZE-1 downto 0); -- status register output
	 
        signal IORegInEn     :   std_logic;                    
        signal IORegOutEn    :   std_logic;                      
        signal IORegOut      :   std_logic_vector(7 downto 0);   -- output register bus
        signal SRegOut       :   std_logic_vector(7 downto 0);   -- status register output 
		  

			-- to registers
        signal RegWEn      : std_logic; -- register write enable 
        signal RegWSel     : std_logic_vector(4 downto 0); -- register write select
        signal RegSelA     : std_logic_vector(4 downto 0); -- register A select
        signal RegSelB     : std_logic_vector(4 downto 0); -- register B select
        signal RegDataSel  : std_logic_vector(3 downto 0); 
	 
	 
		  signal bitmask : std_logic_vector(7 downto 0); -- mask for writing to flags
		  signal K	: std_logic_vector(7 downto 0); 
		  

	 begin 
	 UUTALU: entity work.ALU
		port map(
		      ALUOp   => ALUOp,
            ALUSel  => ALUSel,
    
            RegA    => OperandA, 
            RegB    => OperandB,
            
            RegOut   => Result,
            StatusOut    => ALUStatusOut
		);
		
	 UUTIO: entity work.IOReg
    port map(
        RegIn       => OperandA, -- register not included
        RegInSel    => K(6 downto 5) & K(3 downto 0),
        StatusIn    => ALUStatusOut,
        Clk         => clock, 
        RegInEn     => IORegInEn,                     
        RegOutEn    => IORegOutEn,
        bitmask     => FlagMask,
        RegOut      => IORegOut,
        SRegOut     => SRegOut
    ); 
	 
	 StatReg <= SRegOut;
	 
	 
	 UUTCU: entity work.CU
    port map(
        IR  => IR,
        SReg    => SRegOut, 
		  
        ------------------------ unused, to registers
        RegWEn      => RegWEn, 
        RegWSel     => RegWSel,
        RegSelA     => RegSelA,
        RegSelB     => RegSelB,
        RegDataSel  => RegDataSel,

        -- to ALU and SReg
        ALUOp   => ALUOp,
        ALUSel  => ALUSel,
        bitmask => bitmask,
        
        -- I/O
        IORegInEn   => IORegInEn,                      
        IORegOutEn  => IORegOutEn,                     
        SRegOut     => SRegOut, 
        K           => K,
		  Clk			=> clock
        
    );

end ALUTB;
