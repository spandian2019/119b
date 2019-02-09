----------------------------------------------------------------------------
--
--  Test Bench for ALU
--
--  This is a test bench for the ALU entity. The test bench
--  tests the entity by exercising it and checking the ALU result and SReg
--  through the use of arrays of test values. It tests the major operations
--  for the ALU. The test bench entity is called ALUTB.
--
--
--  Revision History:
--  01/30/2019 Sophia Liu Initial revision
--  02/01/2019 Sophia Liu Updated comments
--
----------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

use work.opcodes.all;

entity MemTB is
    -- constants for testing
    constant CLK_PERIOD : time := 50 ns;
    constant TEST_SIZE : natural := 47;

    --constant EDGE_TEST_SIZE: natural := 5;
	 --constant COMMAND_TEST_SIZE: natural := 7;
end MemTB;

architecture TB_ARCHITECTURE of MemTB is

    -- test component declarations

    component  MEM_TEST  is
        port (
            IR      :  in     opcode_word;                      -- Instruction Register
            ProgDB  :  in     std_logic_vector(15 downto 0);    -- second word of instruction
            Reset   :  in     std_logic;                        -- system reset signal (active low)
            clock   :  in     std_logic;                        -- system clock
            DataAB  :  out    std_logic_vector(15 downto 0);    -- data address bus
            DataDB  :  inout  std_logic_vector(7 downto 0);     -- data data bus
            DataRd  :  out    std_logic;                        -- data read (active low)
            DataWr  :  out    std_logic                         -- data write (active low)
        );
    end component;

    -- Signal used to stop clock signal generators
    signal  END_SIM  :  BOOLEAN := FALSE;

    -- Stimulus signals - signals mapped to the input and inout ports of tested entity

    signal IR : opcode_word; -- instruction register input
    signal ProgDB  :  std_logic_vector(15 downto 0);    -- second word of instruction
    signal Reset   :  std_logic;                        -- system reset signal (active low)
    signal clock   :  std_logic;                        -- system clock
    signal DataAB  :  std_logic_vector(15 downto 0);    -- data address bus
    signal DataDB  :  std_logic_vector(7 downto 0);     -- data data bus
    signal DataRd  :  std_logic;                        -- data read (active low)
    signal DataWr  :  std_logic;                         -- data write (active low)

    signal Clk     : std_logic; -- system clock

	type IRVector is array(natural range <>) of opcode_word;
    signal IRTest : IRVector(TEST_SIZE downto 0); -- IR test input

    type DB16Vector is array(natural range <>) of std_logic_vector(15 downto 0);
	signal ProgDBTest : DB16Vector(TEST_SIZE downto 0); -- ProbDB test input
    signal DataABTest : DB16Vector(TEST_SIZE downto 0); -- DataAB expected output

    type DB8Vector is array(natural range <>) of std_logic_vector(7 downto 0);
	signal DataDBWrTest : DB8Vector(TEST_SIZE downto 0); -- DataDb expected write output
    signal DataDBRdTest : DB8Vector(TEST_SIZE downto 0); -- DataDB test read input

    type CheckVector is array(TEST_SIZE downto 0) of std_logic;
    signal DataCheck : CheckVector; -- '1' to check data ab
    signal DataRdCheck: CheckVector; -- '1' to input data db
    signal ProgDBIn : CheckVector;-- '1' to input prog db
    begin
			-- ALU test component
        UUT: MEM_TEST
        port map(
            IR      => IR,
            ProgDB  => ProgDB,
            Reset   => Reset,
            clock   => Clk,
            DataAB  => DataAB,
            DataDB  => DataDB,
            DataRd  => DataRd,
            DataWr  => DataWr
        );

        -- generate the stimulus and test the design
        TB: process
            variable i : integer;
            variable j : integer;
        begin
            -- assign test vectors
--            -- test instructions
--            "LDI R27, $01", "LDI R26, $23", "LD R6, X+", "LD R1, X", "LD R2 -X", -- X = $0123 -- 26, 27 => +16
--            "STS $FFFF, R6", "STS $2222, R1", "STS $5678, R2" -- check loaded regs correctly
--            "LDI R29, $55", "LDI R28, $55", "LD R6, Y+", "LDD R1, Y+5", "LD R2 -Y", -- Y = $5555
--            "STS $0000, R6", "STS $0001, R1", "STS $0002, R2"
--            "LDI R31, $EF", "LDI R30, $A0", "LD R6, Z+", "LDD R1, Z+9", "LD R2 -Z", -- Z = EFA0
--            "ST X, R6", "ST X, R1", "ST X, R2",
--            "LDI R4, $88",
--            "STS $ABCD, R4", "LDS R5, $ABCD", -- sts -> (m) = R4
--            "LDI R2, $EE", "MOV R3, R2" -- R3 = R2 = $EE
--            "ST X+, R3", "ST X, R2", "ST -X, R3",
--            "LDI R2, $00", "ST Y+, R2", "STD Y+10, R2", "ST -Y, R2",
--            "LDI R2, $56", "ST Z+, R2", "STD Z+3, R2", "ST -Z, R2",
--            "LDI R1, $FF", "LDI R3, $11"
--            "PUSH R1", "PUSH R2", "PUSH R3",
--            "POP R3", "POP R2", "POP R1"

            IRTest <= (
            "1110000010110001","1110001010100011","1001000001101101","1001000000011100","1001000000101110",
            "1001001001100000","1001001000010000","1001001000100000",
            "1110010111010101","1110010111000101","1001000001101001","1000000000011000","1001000000101010",
            "1001001001100000","1001001000010000","1001001000100000",
            "1110111011111111","1110101011100000","1001000001100001","1000000000010000","1001000000100010",
            "1001001001101100","1001001000011100","1001001000101100",
            "1110100001001000",
            "1001001001000000","1001000001010000",
            "1110111000101110","0010110000110010",
            "1001001000101101","1001001000101100","1001001000101110",
            "1110000000100000","1001001000101001","1000011000101010","1001001000101010",
            "1110010100101010","1001001000100001","1000001000100011","1001001000100010",
            "1110111100011111","1110000100100001",
            "1001001000011111","1001001000101111","1001001000111111",
            "1001000000111111","1001000000101111","1001000000011111");

            ProgDBTest <= (
            "----------------", "----------------", "----------------", "----------------", "----------------",
            X"FFFF", X"2222", X"5678",
            "----------------", "----------------", "----------------", "----------------", "----------------",
            X"0000", X"0001", X"0002",
            "----------------", "----------------", "----------------", "----------------", "----------------",
            "----------------", "----------------", "----------------",
            "----------------",
            X"ABCD", X"ABCD",
            "----------------", "----------------",
            "----------------", "----------------", "----------------",
            "----------------", "----------------", "----------------", "----------------",
            "----------------", "----------------", "----------------", "----------------",
            "----------------", "----------------",
            "----------------", "----------------", "----------------",
            "----------------", "----------------", "----------------");

            ProgDBIn <= (
            '0', '0', '0', '0', '0',
            '1', '1', '1',
            '0', '0', '0', '0', '0',
            '1', '1', '1',
            '0', '0', '0', '0', '0',
            '0', '0', '0',
            '0',
            '1', '1',
            '0', '0',
            '0', '0', '0',
            '0', '0', '0', '0',
            '0', '0', '0', '0',
            '0', '0', 
            '0', '0', '0',
            '0', '0', '0');

            DataABTest <= (
            "----------------", "----------------", X"0123", X"0124", X"0123",
            X"FFFF", X"2222", X"5678",
            "----------------","----------------", X"5555", X"555B", X"5555",
            X"0000", X"0001", X"0002",
            "----------------", "----------------", X"EFA0", X"EFAA", X"EFA0",
            X"0123", X"0123", X"0123",
            "----------------",
            X"ABCD", X"ABCD",
            "----------------", "----------------",
            X"0123", X"0124", X"0123",
            "----------------", X"5555", X"5560", X"5555",
            "----------------", X"EFA0", X"EFA4", X"EFA0",
            "----------------", "----------------",
             X"FFFF", X"0000",X"0001", -- TODO stack addr?
             X"0001", X"0000", X"FFFF");

            DataDBWrTest <= (
            "--------", "--------", "--------", "--------", "--------",
            X"01", X"02", X"01",
            "--------", "--------", "--------", "--------", "--------",
            X"04", X"05", X"04",
            "--------", "--------", "--------", "--------", "--------",
            X"07", X"08", X"07",
            "--------",
            X"88", "--------",
            "--------", "--------",
            X"EE", X"EE", X"EE",
            "--------", X"00", X"00", X"00",
            "--------", X"56", X"56", X"56",
            "--------", "--------",
            X"FF", X"56", X"11",
            "--------", "--------", "--------");

            DataDBRdTest <= (
             "--------", "--------", X"01", X"02", X"01",
            "--------", "--------", "--------",
            "--------", "--------", X"04", X"05", X"04",
            "--------", "--------", "--------",
            "--------", "--------", X"07", X"08", X"07",
            "--------", "--------", "--------",
            "--------",
            "--------", X"88",
            "--------", "--------",
            "--------", "--------", "--------",
            "--------", "--------", "--------", "--------",
            "--------", "--------", "--------", "--------",
            "--------", "--------",
            "--------", "--------", "--------",
            X"11", X"56", X"FF");

            DataRdCheck <= (
            '0', '0', '1' , '1', '1',
            '0', '0', '0',
            '0', '0', '1' , '1', '1',
            '0', '0', '0',
            '0', '0', '1' , '1', '1',
            '0', '0', '0',
            '0',
            '0', '1',
            '0', '0',
            '0', '0', '0',
            '0', '0', '0', '0',
            '0', '0', '0', '0',
            '0', '0',
            '0', '0', '0',
            '1', '1', '1');

            DataCheck <= (
            '0', '0', '1' , '1', '1',
            '1', '1', '1',
            '0', '0', '1' , '1', '1',
            '1', '1', '1',
            '0', '0', '1' , '1', '1',
            '1', '1', '1',
            '0',
            '1', '1',
            '0', '0',
            '1', '1', '1',
            '0', '1', '1', '1',
            '0', '1', '1', '1',
            '0', '0',
            '1', '1', '1',
            '1', '1', '1');

        	-- initially everything is 0, have not started
            Reset <= '1'; -- begin with reset
			IR      <= (others => '0');
            ProgDB  <= (others => 'Z');
        	wait for CLK_PERIOD*5; -- wait for a bit
            Reset <= '0'; -- de-assert reset
            wait for CLK_PERIOD*0.7;
			-- loop through test vector
			wait for CLK_PERIOD*0.2; 
			for i in TEST_SIZE downto 0 loop
				IR <= IRTest(i);
                ProgDB <= (others => 'Z');

                wait for CLK_PERIOD;
                -- output progDB, if necessary
                if ProgDBIn(i) = '1' then
                    ProgDB<= ProgDBTest(i);
                    wait for CLK_PERIOD;
                    ProgDB <= (others => 'Z');
                end if;

                -- check address, data busses
                if DataCheck(i) = '1' then
                    -- check address bus
					assert (DataABTest(i) = DataAB)
						report  "DataAB failure at test number " & integer'image(j)
						severity  ERROR;

                    -- wait for data rd/wr to check?
                    wait for CLK_PERIOD/2;
                    if DataRdCheck(i) = '1' then
                        DataDB <= DataDBRdTest(i); -- input data db
                        -- make sure rd/wr signals correct
                        assert (DataRd = '0')
    						report  "DataRd DataDBRd failure at test number " & integer'image(j)
    						severity  ERROR;
                        --
                        assert (DataWr = '1')
    						report  "DataWr DataDBRd at test number " & integer'image(j)
    						severity  ERROR;
                    else
                        -- check data db output
                        assert (DataDBWrTest(i) = DataDB)
    						report  "DataDB failure at test number " & integer'image(j)
    						severity  ERROR;
                        -- make sure rd/wr signals correct
                        assert (DataRd = '1')
    						report  "DataRd DataDBWr failure at test number " & integer'image(j)
    						severity  ERROR;
                        --
                        assert (DataWr = '0')
    						report  "DataWr DataDBWr at test number " & integer'image(j)
    						severity  ERROR;
                    end if;
                    wait for CLK_PERIOD/2; -- wait for rest of clock
                end if;


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

configuration TESTBENCH_FOR_MEM of MemTB is
    for TB_ARCHITECTURE
		  for UUT : MEM_TEST
            use entity work.MEM_TEST;
        end for;
    end for;
end TESTBENCH_FOR_MEM;
