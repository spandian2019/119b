----------------------------------------------------------------------------
--
--  Test Bench for Registers
--
--  This is a test bench for the SerialDivider entity. The test bench
--  thoroughly tests the entity by exercising it and checking the outputs
--  through the use of an array of test values (TestVector). The test bench
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
            
            RegOut  : out std_logic_vector(REGSIZE-1 downto 0); -- output result
            StatusOut    : out std_logic_vector(REGSIZE-1 downto 0) -- status register output
        );
    end component;
    
    -- Signal used to stop clock signal generators
    signal  END_SIM  :  BOOLEAN := FALSE;
    
    -- Stimulus signals - signals mapped to the input and inout ports of tested entity
    signal Clk     : std_logic; -- system clock
    signal ALUOp   : std_logic_vector(3 downto 0); -- operation control signals 
    signal ALUSel  : std_logic_vector(1 downto 0); -- operation select 
    signal RegA    : std_logic_vector(REGSIZE-1 downto 0); -- operand A
    signal RegB    : std_logic_vector(REGSIZE-1 downto 0); -- operand B, or immediate   
    signal RegOut  : std_logic_vector(REGSIZE-1 downto 0); -- output result
    signal StatusOut    : std_logic_vector(REGSIZE-1 downto 0); -- status register output
    
    type VectorCases is array (integer range <>) of std_logic_vector(REGSIZE-1 downto 0);
    signal EdgeCasesA : VectorCases(EDGE_TEST_SIZE downto 0);
    signal EdgeCasesB : VectorCases(EDGE_TEST_SIZE downto 0);
    signal AddResult : VectorCases(EDGE_TEST_SIZE downto 0);
    signal AddFlags : VectorCases(EDGE_TEST_SIZE downto 0);
    signal SubResult : VectorCases(EDGE_TEST_SIZE downto 0);
    signal SubFlags : VectorCases(EDGE_TEST_SIZE downto 0);
    
    signal TestResult : std_logic_vector(REGSIZE-1 downto 0); 
    begin
        UUT: ALU 
        port map(
            --Clk     => Clk,
            ALUOp   => ALUOp,
            ALUSel  => ALUSel,
            RegA    => RegA,
            RegB    => RegB,
            RegOut  => RegOut,
            StatusOut    => StatusOut
        );
        
        -- generate the stimulus and test the design
        TB: process
        variable  i  :  integer; 
        begin 
            EdgeCasesA <= (X"00", X"FF", X"EE", X"80", X"01", X"34");
            EdgeCasesB <= (X"00", X"FF", X"11", X"80", X"05", X"F0");
            
            AddResult <= (X"00", X"FE", X"FF", X"00", X"06", X"24");
            AddFlags <= ("--000010", "--110101", "--010100", "--011011", "--000000", "--000001");
            
            SubResult <= (X"00", X"00", X"DD", X"00", X"FC", X"44");
            SubResult <= ("--000010", "--000010", "--010100", "--000010", "--110101", "--000001");
            
            -- initially everything is X, have not started
            ALUOp   <= "XXXX";
            ALUSel  <= "XX";
            RegA    <= "XXXXXXXX";
            RegB    <= "XXXXXXXX";
            wait for 100 ns; 
            
            -- test add/subtract
            -- add no carry
            for i in 0 to EdgeCasesA'LENGTH-1 loop 
               ALUOp <= OP_ADD and OP_NOCARRY; 
               ALUSel <= ADDSUBEN; 
               RegA <= EdgeCasesA(i);
               RegB <= EdgeCasesB(i); 
               
               wait for CLK_PERIOD; 
               -- check result
               assert (std_match(AddResult(i), RegOut))
                    report  "Add result failure"
                    severity  ERROR;
               -- check pre-masked sreg
               assert (std_match(AddFlags(i), StatusOut))
                    report  "Add  sreg failure"
                    severity  ERROR;
            end loop; 
            
            -- sub no carry --TODO put together
            for i in 0 to EdgeCasesA'LENGTH-1 loop 
               ALUOp <= OP_SUB and OP_NOCARRY; 
               ALUSel <= ADDSUBEN; 
               RegA <= EdgeCasesA(i);
               RegB <= EdgeCasesB(i); 
               
               wait for CLK_PERIOD; 
               -- check result
               assert (std_match(SubResult(i), RegOut))
                    report  "Add result failure"
                    severity  ERROR;
               -- check pre-masked sreg
               assert (std_match(SubFlags(i), StatusOut))
                    report  "Add  sreg failure"
                    severity  ERROR;
            end loop;

            -- add no carry 
            -- add cary 
            -- subtract carry 
            -- subtract no carry
            
            -- test logical operations  
            -- nor 
            -- not 
            -- xor 
            -- nand 
            -- and 
            -- xor 
            -- or 
            -- 0, 1??? 
    
            -- test shift 
            -- asr 
            -- lsr 
            -- ror
            
            
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