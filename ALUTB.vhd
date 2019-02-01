----------------------------------------------------------------------------
--
--  Test Bench for ALU
--
--  This is a test bench for the ALU entity. The test bench
--  thoroughly tests the entity by exercising it and checking the outputs
--  through the use of an array of test values. The test bench
--  entity is called ALUTB.
--  
--
--  Revision History:
--  01/30/2019 Sophia Liu Initial revision  
--
----------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;
 
use work.opcodes.all; 

use work.ALUconstants.all; 

entity ALUTB is
    -- timing constants for testing  
    constant CLK_PERIOD : time := 20 ns;
    constant EDGE_TEST_SIZE: natural := 5; 
end ALUTB;

architecture TB_ARCHITECTURE of ALUTB is

    -- test component declaration 
    component ALU is
        port(
            --Clk     : in std_logic; -- system clock
            ALUOp   : in std_logic_vector(3 downto 0); -- operation control signals 
            ALUSel  : in std_logic_vector(1 downto 0); -- operation select 
    
            RegA    : in std_logic_vector(REGSIZE-1 downto 0); -- operand A
            RegB    : in std_logic_vector(REGSIZE-1 downto 0); -- operand B, or immediate 
            
				FlagMask: out std_logic_vector(REGSIZE - 1 downto 0); -- mask for writing to status flags
            RegOut  : out std_logic_vector(REGSIZE-1 downto 0); -- output result
            StatusOut    : out std_logic_vector(REGSIZE-1 downto 0) -- status register output
        );
    end component;
    
    -- Signal used to stop clock signal generators
    signal  END_SIM  :  BOOLEAN := FALSE;
    
    -- Stimulus signals - signals mapped to the input and inout ports of tested entity
    signal Clk     : std_logic; -- system clock
    signal ALUOp   : std_logic_vector(3 downto 0) := "0000"; -- operation control signals 
    signal ALUSel  : std_logic_vector(1 downto 0) := "00"; -- operation select 
    signal RegA    : std_logic_vector(REGSIZE-1 downto 0) := "00000000"; -- operand A
    signal RegB    : std_logic_vector(REGSIZE-1 downto 0) := "00000000"; -- operand B, or immediate   
    signal RegOut  : std_logic_vector(REGSIZE-1 downto 0); -- output result
    signal StatusOut    : std_logic_vector(REGSIZE-1 downto 0); -- status register output
	 signal FlagMask    : std_logic_vector(REGSIZE-1 downto 0); -- status register output
    
    type VectorCases is array (EDGE_TEST_SIZE downto 0) of std_logic_vector(REGSIZE-1 downto 0);
	signal EdgeCasesA : VectorCases; 
	signal EdgeCasesB : VectorCases;
	
	type TestVector is array (natural range <>) of VectorCases;
	signal TestResult : TestVector(9 downto 0);
	signal TestFlags : TestVector(9 downto 0);
	
	type OpCases is array (integer range <>) of std_logic_vector(3 downto 0);
	signal TestOp : OpCases(9 downto 0);  
	
	type SelCases is array (integer range <>) of std_logic_vector(1 downto 0);
	signal TestSel : SelCases(9 downto 0); 
	
    begin
        UUT: ALU 
        port map(
            --Clk     => Clk,
            ALUOp   => ALUOp,
            ALUSel  => ALUSel,
            RegA    => RegA,
            RegB    => RegB,
				--FlagMask => FlagMask,
            RegOut  => RegOut,
            StatusOut    => StatusOut
        );
        
        -- generate the stimulus and test the design
        TB: process
        variable  i  :  integer; 
		  variable j : integer;
        begin 
        	EdgeCasesA <= (X"00", X"FF", X"EE", X"80", X"01", X"34");
        	EdgeCasesB <= (X"00", X"FF", X"11", X"80", X"05", X"F0");
        	
			TestResult <= ((X"00", X"FE", X"FF", X"00", X"06", X"24"), --add (no carry)
			(X"00", X"00", X"DD", X"00", X"FC", X"44"), -- sub  (no carry)
			(X"00", X"FF", X"00", X"80", X"01", X"30"), -- and
			(X"FF", X"00", X"11", X"7F", X"FE", X"CB"), -- not 
			(X"00", X"00", X"FF", X"00", X"04", X"C4"), -- xor 
			(X"00", X"FF", X"FF", X"80", X"05", X"F4"), -- or 
			(X"00", X"FF", X"F7", X"C0", X"00", X"1A"), -- asr 
			(X"00", X"7F", X"77", X"40", X"00", X"1A"), -- lsr 
			(X"00", X"7F", X"77", X"40", X"00", X"1A"), -- ror 
			(X"80", X"FF", X"F7", X"C0", X"80", X"9A")  -- rorc
			);
			
			TestFlags <= (("--000010", "--110101", "--010100", "--011011", "--000000", "--000001"), 
        	("--000010", "--000010", "--010100", "--000010", "--110101", "--000001"),
			("---0001-", "---1010-", "---0001-", "---1010-", "---0000-", "---0000-"),
			("---10101", "---00011", "---00001", "---00001", "---10101", "---10101"),
			("---0001-", "---0001-", "---1010-", "---0001-", "---0000-", "---1010-"),
			("---0001-", "---1010-", "---1010-", "---1010-", "---0000-", "---1010-"),
			("---00010", "---10101", "---01100", "---01100", "---11011", "---00000"),
         ("---00010", "---11001", "---00000", "---00000", "---11011", "---00000"),
          ("---00010", "---11001", "---00000", "---00000", "---11011", "---00000"),
		   ("---01100", "---10101", "---01100", "---01100", "---10101", "---01100"));
			
			TestOp <= (OP_ADDNC, OP_SUBNC, OP_AND, OP_NOTA, OP_XOR, OP_OR, OP_ASR, OP_LSR, OP_ROR, OP_RORC);
			
			TestSel <= (ADDSUBEN, ADDSUBEN, FBLOCKEN, FBLOCKEN, FBLOCKEN, FBLOCKEN, SHIFTEN, SHIFTEN, SHIFTEN, SHIFTEN);
	 
--        	-- initially everything is 0, have not started
--            ALUOp   <= "0000";
--            ALUSel  <= "00";
--            RegA    <= "00000000";
--            RegB    <= "00000000";
        	wait for 100 ns; -- wait for a bit 
        	
			-- loop through test vector
			for j in 9 downto 0 loop 
				for i in EDGE_TEST_SIZE downto 0 loop 
					ALUOp <= TestOp(j);
					ALUSel <= TestSel(j); 
					RegA <= EdgeCasesA(i);
					RegB <= EdgeCasesB(i); 
					
					wait for CLK_PERIOD; 
					
					-- check result
					assert (std_match(TestResult(j)(i), RegOut))
						report  "result failure " & integer'image(j)
						severity  ERROR;
						
					-- check pre-masked sreg
					assert (std_match(TestFlags(j)(i), StatusOut))
						report  "sreg failure " & integer'image(j)
						severity  ERROR;
				end loop;
			end loop; 
				
            
            END_SIM <= TRUE;        -- end of stimulus events
            wait;                   -- wait for simulation to end
        end process; 

        -- process for generating system clock
        CLOCK_CLK : process
        begin
            -- this process generates a 20 ns 50% duty cycle clock
            -- stop the clock when the end of the simulation is reached
            if END_SIM = FALSE then
                CLK <= '0';
                wait for CLK_PERIOD/2;
            else
                wait;
            end if;
    
            if END_SIM = FALSE then
                CLK <= '1';
                wait for CLK_PERIOD/2;
            else
                wait;
            end if;
        end process;

end TB_ARCHITECTURE;

configuration TESTBENCH_FOR_ALU of ALUTB is
    for TB_ARCHITECTURE 
		  for UUT : ALU
            use entity work.ALU;
        end for;
    end for;
end TESTBENCH_FOR_ALU;