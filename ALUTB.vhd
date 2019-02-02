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
	 constant COMMAND_TEST_SIZE: natural := 7; 
end ALUTB;

architecture TB_ARCHITECTURE of ALUTB is

    -- test component declaration 
    component ALU is
        port(
            ALUOp   : in std_logic_vector(3 downto 0); -- operation control signals 
            ALUSel  : in std_logic_vector(2 downto 0); -- operation select 
    
            RegA    : in std_logic_vector(REGSIZE-1 downto 0); -- operand A
            RegB    : in std_logic_vector(REGSIZE-1 downto 0); -- operand B, or immediate 
            
            RegOut  : out std_logic_vector(REGSIZE-1 downto 0); -- output result
            StatusOut    : out std_logic_vector(REGSIZE-1 downto 0) -- status register output
        );
    end component;
    
	 component ALU_TEST  is
    port(
        IR        :  in  opcode_word;                       -- Instruction Register
        OperandA  :  in  std_logic_vector(7 downto 0);      -- first operand
        OperandB  :  in  std_logic_vector(7 downto 0);      -- second operand
        clock     :  in  std_logic;                         -- system clock
        Result    :  out std_logic_vector(7 downto 0);      -- ALU result
        StatReg   :  out std_logic_vector(7 downto 0)       -- status register
    );
	end component;


    -- Signal used to stop clock signal generators
    signal  END_SIM  :  BOOLEAN := FALSE;
    
    -- Stimulus signals - signals mapped to the input and inout ports of tested entity
	 -- ALU entity 
    signal Clk     : std_logic; -- system clock
    signal ALUOp   : std_logic_vector(3 downto 0) := "0000"; -- operation control signals 
    signal ALUSel  : std_logic_vector(2 downto 0) := "000"; -- operation select 
    signal RegA    : std_logic_vector(REGSIZE-1 downto 0) := "00000000"; -- operand A
    signal RegB    : std_logic_vector(REGSIZE-1 downto 0) := "00000000"; -- operand B, or immediate   
    signal RegOut  : std_logic_vector(REGSIZE-1 downto 0); -- output result
    signal StatusOut    : std_logic_vector(REGSIZE-1 downto 0); -- status register output
	 
	 -- ALU_TEST entity 
	 signal IR : opcode_word; -- instruction register input
	 signal Result : std_logic_vector(REGSIZE-1 downto 0); -- output result 
	 signal StatReg : std_logic_vector(REGSIZE-1 downto 0); -- status register output
	 
    -- test vectors 
    type VectorCases is array (EDGE_TEST_SIZE downto 0) of std_logic_vector(REGSIZE-1 downto 0);
	signal EdgeCasesA : VectorCases; 
	signal EdgeCasesB : VectorCases;
	
	type TestVector is array (natural range <>) of VectorCases;
	signal TestResult : TestVector(COMMAND_TEST_SIZE downto 0);
	signal TestFlags : TestVector(COMMAND_TEST_SIZE downto 0);
	
	type OpCases is array (natural range <>) of ALU_OPS;
	signal TestOp : OpCases(COMMAND_TEST_SIZE downto 0);  
	
	type SelCases is array (natural range <>) of ALU_SELECTS;
	signal TestSel : SelCases(COMMAND_TEST_SIZE downto 0); 
	
	type IRVector is array(natural range <>) of opcode_word; 
	signal IRTest : IRVector(COMMAND_TEST_SIZE downto 0);
	
	signal IRTBitTest : IRVector(EDGE_TEST_SIZE downto 0); 
	signal TCasesA : VectorCases;
	signal TCasesB : VectorCases;
	signal TTestResult : VectorCases;
	signal TTestFlags : VectorCases; 

    begin
			-- ALU test component
        UUT: ALU 
        port map(
            ALUOp   => ALUOp,
            ALUSel  => ALUSel,
            RegA    => RegA,
            RegB    => RegB,
            RegOut  => RegOut,
            StatusOut    => StatusOut
        );
		  
		  -- ALU + CU test component
		  UUT2: ALU_TEST
		  port map(
        IR        => IR,          -- Instruction Register
        OperandA  => RegA,     -- first operand
        OperandB  => RegB,      -- second operand
        clock     => clk,        -- system clock
        Result    => Result,      -- ALU result
        StatReg   => StatReg      -- status register
			);
        
        -- generate the stimulus and test the design
        TB: process
        variable  i  :  integer; 
		  variable j : integer;
        begin 
        	EdgeCasesA <= (X"00", X"FF", X"EE", X"80", X"01", X"34");
        	EdgeCasesB <= (X"00", X"FF", X"11", X"80", X"05", X"F0");
			
			IRTest <= (OpADD, OpSUB, OpAND, OpEOR, OpOr, OpASR, OpLSR, OpROR);
        	
			TestResult <= ((X"00", X"FE", X"FF", X"00", X"06", X"24"), --add (no carry)
			(X"00", X"00", X"DD", X"00", X"FC", X"44"), -- sub  (no carry)
			(X"00", X"FF", X"00", X"80", X"01", X"30"), -- and
			--(X"FF", X"00", X"11", X"7F", X"FE", X"CB"), -- com -- performs subtract 
			(X"00", X"00", X"FF", X"00", X"04", X"C4"), -- xor 
			(X"00", X"FF", X"FF", X"80", X"05", X"F4"), -- or 
			(X"00", X"FF", X"F7", X"C0", X"00", X"1A"), -- asr 
			(X"00", X"7F", X"77", X"40", X"00", X"1A"), -- lsr 
			--(X"00", X"7F", X"77", X"40", X"00", X"1A"), -- ror -- used with ALU-only testing
			--(X"80", X"FF", X"F7", X"C0", X"80", X"9A")  -- rorc
			(X"00", X"7F", X"F7", X"40", X"00", X"9A")	-- ror for ALU_TEST 
			);
			
			TestFlags <= (("--000010", "--110101", "--010100", "--011011", "--000000", "--000001"), 
        	("--000010", "--000010", "--010100", "--000010", "--110101", "--000001"),
			("---0001-", "---1010-", "---0001-", "---1010-", "---0000-", "---0000-"),
			--("---10101", "---00011", "---00001", "---00001", "---10101", "---10101"),
			("---0001-", "---0001-", "---1010-", "---0001-", "---0000-", "---1010-"),
			("---0001-", "---1010-", "---1010-", "---1010-", "---0000-", "---1010-"),
			("---00010", "---10101", "---01100", "---01100", "---11011", "---00000"),
         ("---00010", "---11001", "---00000", "---00000", "---11011", "---00000"),
          --("---00010", "---11001", "---00000", "---00000", "---11011", "---00000"), -- ror for ALU-only 
		   --("---01100", "---10101", "---01100", "---01100", "---10101", "---01100"),
			("---00010", "---11001", "---01100", "---00000", "---11011", "---01100")  -- ror for ALU_TEST
			);
			
			TestOp <= (OP_ADDNC, OP_SUBNC, OP_AND, OP_XOR, OP_OR, OP_ASR, OP_LSR, OP_ROR); 
			
			TestSel <= (ADDSUBEN, ADDSUBEN, FBLOCKEN, FBLOCKEN, FBLOCKEN, SHIFTEN, SHIFTEN, SHIFTEN);
			
			-- test BST, BLD
			IRTBitTest <= (OpBST, OpBST, OpBST, OpBLD, OpBST, OpBLD); 
			TCasesA <= (X"00", X"FF", X"34", X"00", X"00", X"FF");
        	TCasesB <= ("00000000", "00000111", "00000101", "00000000", "00000000", "00000111");
			TTestResult <= ("--------", "--------", "--------", x"01", "--------", x"7F");
			TTestFlags <= ("-0------", "-1------", "-1------", "--------", "-0------", "--------");
	 
        	-- initially everything is 0, have not started
            ALUOp   <= "0000";
            ALUSel  <= "000";
            RegA    <= "00000000";
            RegB    <= "00000000";
				IR      <= "0000000000000000";
        	wait for 100 ns; -- wait for a bit 
        	
			-- loop through test vector
			-- test ALU only (ALU entity)
			for j in COMMAND_TEST_SIZE downto 1 loop 
				for i in EDGE_TEST_SIZE downto 0 loop 
					ALUOp <= TestOp(j);
					ALUSel <= TestSel(j); 
					RegA <= EdgeCasesA(i);
					RegB <= EdgeCasesB(i); 
					
					wait for CLK_PERIOD*0.8; -- wait for most of clock cycle
					
					-- check result near end of clock
					assert (std_match(TestResult(j)(i), RegOut))
						report  "result failure " & integer'image(j)
						severity  ERROR;
						
					-- check pre-masked sreg
					assert (std_match(TestFlags(j)(i), StatusOut))
						report  "sreg failure " & integer'image(j)
						severity  ERROR;
					
					wait for CLK_PERIOD*0.2; -- wait for rest of clock cycle 
				end loop;
			end loop; 
				
				-- test ALU with control unit and sreg (ALU_TEST entity)
            wait for CLK_PERIOD*5.5;
				

			for j in COMMAND_TEST_SIZE downto 0 loop 
            for i in EDGE_TEST_SIZE downto 0 loop
                IR <= IRTest(j); 
					 
					RegA <= EdgeCasesA(i);
					RegB <= EdgeCasesB(i); 
					
                wait for CLK_PERIOD*0.9;
					 
					-- check result near end of clock
					assert (std_match(TestResult(j)(i), Result))
						report  "result failure " & integer'image(j)
						severity  ERROR;
						
					-- check pre-masked sreg
					assert (std_match(TestFlags(j)(i), StatReg))
						report  "sreg failure " & integer'image(j)
						severity  ERROR;
						  
                wait for CLK_PERIOD*0.1;
            end loop;
				end loop; 
				
				for i in EDGE_TEST_SIZE downto 0 loop
                IR <= IRTBitTest(i); 
					 
					RegA <= TCasesA(i);
					RegB <= TCasesB(i); 
					
                wait for CLK_PERIOD*0.9;
					 
					-- check result near end of clock
					assert (std_match(TTestResult(i), Result))
						report  "tbit result failure " & integer'image(i)
						severity  ERROR;
						
					-- check pre-masked sreg
					assert (std_match(TTestFlags(i), StatReg))
						report  "tbit sreg failure " & integer'image(i)
						severity  ERROR;
						  
                wait for CLK_PERIOD*0.1;
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
		  for UUT2 : ALU_TEST
            use entity work.ALU_TEST;
        end for;
    end for;
end TESTBENCH_FOR_ALU;