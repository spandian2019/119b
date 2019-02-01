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
            signal ALUSel  : std_logic_vector(1 downto 0); -- operation select 
            
				signal FlagMask: std_logic_vector(REGSIZE - 1 downto 0); -- mask for writing to status flags
            signal ALUStatusOut    : std_logic_vector(REGSIZE-1 downto 0); -- status register output

	component ALU is
        port(
            ALUOp   : in std_logic_vector(3 downto 0); -- operation control signals 
            ALUSel  : in std_logic_vector(1 downto 0); -- operation select 
    
            RegA    : in std_logic_vector(REGSIZE-1 downto 0); -- operand A
            RegB    : in std_logic_vector(REGSIZE-1 downto 0); -- operand B, or immediate 
            
				FlagMask: out std_logic_vector(REGSIZE - 1 downto 0); -- mask for writing to status flags
            RegOut  : out std_logic_vector(REGSIZE-1 downto 0); -- output result
            StatusOut    : out std_logic_vector(REGSIZE-1 downto 0) -- status output to sreg
        );
    end component;
	 
	 
		  signal Clk           :    std_logic;                      	-- system clock
        signal IORegInEn     :   std_logic;                    
        signal IORegOutEn    :   std_logic;                      
        signal IORegOut      :   std_logic_vector(7 downto 0);   -- output register bus
        signal SRegOut       :   std_logic_vector(7 downto 0);   -- status register output 
		  
	 component IOReg is
    port(
        RegIn       : in    std_logic_vector(7 downto 0);   -- input register bus
        RegInSel    : in    std_logic_vector(5 downto 0);   -- IO register address bus
        StatusIn    : in    std_logic_vector(7 downto 0);   -- input status flags
        Clk         : in    std_logic;                      -- system clock
        RegInEn     : in    std_logic;                      -- 
        RegOutEn    : in    std_logic;                      --
        bitmask     : in    std_logic_vector(7 downto 0);   --
        RegOut      : out   std_logic_vector(7 downto 0);   -- output register bus
        SRegOut     : out   std_logic_vector(7 downto 0)    --
    );
	 end component; 
	 
	 -------------------- currently unused
			-- to registers
        signal RegWEn      : std_logic; -- register write enable 
        signal RegWSel     : std_logic_vector(4 downto 0); -- register write select
        signal RegSelA     : std_logic_vector(4 downto 0); -- register A select
        signal RegSelB     : std_logic_vector(4 downto 0); -- register B select
        signal RegDataSel  : std_logic_vector(3 downto 0); 
		  
        -- Program memory access
        signal ProgAddr	: std_logic_vector(15 downto 0); -- address source for program memory unit
        signal ProgLoad	: std_logic;                    -- load select for PC
        signal ProgAddrSel : std_logic_vector(1 downto 0);  -- program address source select 
        
        -- Data memory access       
        signal DataAddrSel : std_logic_vector(1 downto 0);  -- data address source select
        signal DataOffsetSel : std_logic_vector(1 downto 0);-- data address offset source select 
        signal PreSel  : std_logic; -- data pre/post address select 
        signal DataRd  : std_logic; -- indicates data memory is being read
        signal DataWr  : std_logic; -- indicates data memory is being written
        signal DataAddr	: std_logic_vector(15 downto 0); -- address source for data memory unit
        signal AddrOffset	: std_logic_vector(5 downto 0); -- address offset for data memory unit 
        
        -- Stack
        signal StackEn     : std_logic; -- stack enable signal 
        signal StackPush   : std_logic; -- stack push/pop control
        signal Reset       : std_logic; -- active low reset signal
	 
	 
		  signal bitmask : std_logic_vector(7 downto 0); -- mask for writing to flags
		  signal K	: std_logic_vector(7 downto 0); 
		  
	 component CU is
    port(
        ProgDB  : in std_logic_vector(15 downto 0); -- program memory data bus
        SReg    : in std_logic_vector(7 downto 0);  -- status flags
        
        -- to registers
        RegWEn      : out std_logic; -- register write enable 
        RegWSel     : out std_logic_vector(4 downto 0); -- register write select
        RegSelA     : out std_logic_vector(4 downto 0); -- register A select
        RegSelB     : out std_logic_vector(4 downto 0); -- register B select
        RegDataSel  : out std_logic_vector(3 downto 0);

        -- to ALU and SReg
        ALUOp   : out std_logic_vector(4 downto 0); -- operation control signals 
        ALUSel  : out std_logic_vector(2 downto 0); -- operation select 
        bitmask : out std_logic_vector(7 downto 0); -- mask for writing to flags
        
        -- I/O
        IORegInEn   : out   std_logic;                      -- 
        IORegOutEn  : out   std_logic;                      --
        SRegOut     : out   std_logic_vector(7 downto 0); 
        K           : out std_logic_vector(7 downto 0); -- immediate value K
        
        
        -- Program memory access
        ProgAddr: out std_logic_vector(15 downto 0); -- address source for program memory unit
        ProgLoad: out std_logic;                    -- load select for PC
        ProgAddrSel : in std_logic_vector(1 downto 0);  -- program address source select 
        
        -- Data memory access       
        DataAddrSel : out std_logic_vector(1 downto 0);  -- data address source select
        DataOffsetSel : out std_logic_vector(1 downto 0);-- data address offset source select 
        PreSel  : out std_logic; -- data pre/post address select 
        DataRd  : out std_logic; -- indicates data memory is being read
        DataWr  : out std_logic; -- indicates data memory is being written
        DataAddr: out std_logic_vector(15 downto 0); -- address source for data memory unit
        AddrOffset: out std_logic_vector(5 downto 0); -- address offset for data memory unit 
        
        -- Stack
        StackEn     : out std_logic; -- stack enable signal 
        StackPush   : out std_logic; -- stack push/pop control
        Reset       : out std_logic -- active low reset signal
    );
	 end component; 

	 begin 
	 UUTALU: ALU
		port map(
		      ALUOp   => ALUOp,
            ALUSel  => ALUSel,
    
            RegA    => OperandA, 
            RegB    => OperandB,
            
				FlagMask => FlagMask, 
            RegOut   => Result,
            StatusOut    => ALUStatusOut
		);
		
	 UUTIO: IOReg 
    port map(
        RegIn       => OperandA, -- register not included
        RegInSel    => K(6 downto 5) & K(3 downto 0),
        StatusIn    => ALUStatusOut,
        Clk         => Clk, 
        RegInEn     => IORegInEn,                     
        RegOutEn    => IORegOutEn,
        bitmask     => FlagMask,
        RegOut      => IORegOut,
        SRegOut     => SRegOut
    ); 
	 
	 StatReg <= SRegOut;
	 
	 
	 UUTCU: CU
    port map(
        ProgDB  => IR,
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
        
        -------------------- currently unused 
        -- Program memory access
        ProgAddr	=> ProgAddr, 
        ProgLoad	=> ProgLoad,
        ProgAddrSel => ProgAddrSel,  
        
        -- Data memory access       
        DataAddrSel => DataAddrSel, 
        DataOffsetSel => DataOffsetSel, 
        PreSel  => PreSel, 
        DataRd  => DataRd, 
        DataWr  => DataWr, 
        DataAddr	=> DataAddr,
        AddrOffset	=> AddrOffset,
		  
        -- Stack
        StackEn     => StackEn, 
        StackPush   => StackPush, 
        Reset       => Reset
    );

end ALUTB;
