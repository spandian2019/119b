----------------------------------------------------------------------------
--
--  Test Bench for data memory interface unit
--
--  This is a test bench for the MEM_TEST entity. The test bench
--  tests the entity by exercising it and checking the data ab and db
--  through the use of arrays of test values. It tests the major operations
--  for the DMIU. The test bench entity is called MemTB.
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
use work.constants.all;

entity MemTB is
    -- constants for testing
    constant CLK_PERIOD : time := 20 ns;
    constant TEST_SIZE : natural := 67;

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

    -- Stimulus signals - signals mapped to the input and output ports of tested entity
    signal IR : opcode_word; -- instruction register input
    signal ProgDB  :  std_logic_vector(15 downto 0);    -- second word of instruction
    signal Reset   :  std_logic;                        -- system reset signal (active low)
    signal DataAB  :  std_logic_vector(15 downto 0);    -- data address bus
    signal DataDB  :  std_logic_vector(7 downto 0);     -- data data bus
    signal DataRd  :  std_logic;                        -- data read (active low)
    signal DataWr  :  std_logic;                        -- data write (active low)

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

    -- for handling in/out dataDB
    signal DataData : std_logic_vector(REGSIZE-1 downto 0);
    signal WriteToDataDB : std_logic;

    begin
	   -- Memory test component
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
--            test instructions: 
--            "LDI R27, $01", "LDI R26, $23", "LD R6, X+", "LD R1, X", "LD R2 -X", -- test ldx +-
--            "STS $FFFF, R6", "STS $2222, R1", "STS $5678, R2" -- check loaded regs correctly
--            "LDI R29, $55", "LDI R28, $55", "LD R6, Y+", "LDD R1, Y+0", "LD R2 -Y", -- test ldy+-
--            "STS $0000, R6", "STS $0001, R1", "STS $0002, R2" -- check loaded regs correctly
--            "LDI R31, $EF", "LDI R30, $A0", "LD R6, Z+", "LDD R1, Z+0", "LD R2 -Z", -- test ldz+-
--            "ST X, R6", "ST X, R1", "ST X, R2", -- check loaded regs correctly
--            "LDD R1, Y+0", "LDD R0, Y+$3F" --  test ldd, std extremes
--            "STD Z+0, R1", "STD Z+$3F, R0"
--            "LDD R1, Z+0", "LDD R0, Z+$3F"
--            "STD Y+0, R1", "STD Y+$3F, R0"
--            "LDI R20, $88", -- test sts, lds 
--            "STS $ABCD, R20", "LDS R21, $ABCD", "LDS R16, $FFFF", --sts -> (m) = R4
--            "MOV R24, R21", "LDI R18, $EE", "MOV R19, R18"-- test mov
--            "LDI R27, $FF", "LDI R26, $FF", -- test stx+- extremes (X=$FFFF)
--            "ST X+, R24", "ST X, R21", "ST -X, R18", "ST -X, R19" -- test stx+-, check mov
--            "LDI R18, $00", "ST Y+, R18", "STD Y+10, R18", "ST -Y, R18", -- test sty+-
--            "LDI R18, $56", "ST Z+, R18", "STD Z+3, R18", "ST -Z, R18", -- test stz+-
--             -- undefined behaviors; doesn't change xyz addr
--            "LD R27, X", "ST X, R18", "LD R28, Y+", "ST -Y, R18", "LD R31, -Z", "ST Z+, R30", "ST Z+, R16" -- loading to/from xyz
--            "LDI R17, $FF", "LDI R19, $11" -- test push/pop
--            "PUSH R17", "PUSH R18", "PUSH R19",
--            "POP R19", "POP R18", "POP R17"

            IRTest <= (
            "1110000010110001","1110001010100011","1001000001101101","1001000000011100","1001000000101110",--ldi,ldx
            "1001001001100000","1001001000010000","1001001000100000",--sts
            "1110010111010101","1110010111000101","1001000001101001","1000000000011000","1001000000101010",--ldi,ldy
            "1001001001100000","1001001000010000","1001001000100000",--sts
            "1110111011111111","1110101011100000","1001000001100001","1000000000010000","1001000000100010",--ldi,ldz
            "1001001001101100","1001001000011100","1001001000101100",--stx
            "1000000000011000","1010110000001111",--lddy
            "1000001000010000","1010111000000111",--stdz
            "1000000000010000","1010110000000111",--lddz
            "1000001000011000","1010111000001111",--stdy
            "1110100001001000",--ldi
            "1001001101000000","1001000101010000","1001000100000000",--sts,lds
            "0010111110000101","1110111000101110","0010111100110010",--ldi,mov
            "1110111110111111","1110111110101111",--ldi
            "1001001110001101","1001001101011100","1001001100101110","1001001100111110",--stx
            "1110000000100000","1001001100101001","1000011100101010","1001001100101010",--ldi,sty
            "1110010100100110","1001001100100001","1000001100100011","1001001100100010",--ldi,stz
            "1001000110111100", "1001001100101100","1001000111001001","1001001100101010","1001000111110010","1001001111100001","1001001100000001",--ld/stxyz
            "1110111100011111","1110000100110001",--ldi
            "1001001100011111","1001001100101111","1001001100111111",--push
            "1001000100111111","1001000100101111","1001000100011111");--pop



            ProgDBTest <= (
            "----------------", "----------------", "----------------", "----------------", "----------------", -- ldi, ldx
            X"FFFF", X"2222", X"5678", -- sts
            "----------------", "----------------", "----------------", "----------------", "----------------", -- ldi, ldy
            X"0000", X"0001", X"0002", -- sts
            "----------------", "----------------", "----------------", "----------------", "----------------", -- ldi, ldz
            "----------------", "----------------", "----------------", -- stx
            "----------------", "----------------", -- lddy
            "----------------", "----------------", -- stdz
            "----------------", "----------------", -- lddz
            "----------------", "----------------", -- stdy
            "----------------", -- ldi
            X"ABCD", X"ABCD", X"FFFF",-- sts, lds
            "----------------", "----------------", "----------------",  -- ldi, mov
            "----------------", "----------------", -- ldi
            "----------------", "----------------", "----------------","----------------", -- stx
            "----------------", "----------------", "----------------", "----------------", -- ldi, sty
            "----------------", "----------------", "----------------", "----------------", -- ldi, stz
            "----------------", "----------------", "----------------", "----------------", "----------------", "----------------", "----------------", --ld/stxyz
            "----------------", "----------------", -- ldi
            "----------------", "----------------", "----------------", -- push
            "----------------", "----------------", "----------------"); -- pop

            ProgDBIn <= (
            '0', '0', '0', '0', '0',
            '1', '1', '1',
            '0', '0', '0', '0', '0',
            '1', '1', '1',
            '0', '0', '0', '0', '0',
            '0', '0', '0',
            '0', '0',
            '0', '0',
            '0', '0',
            '0', '0',
            '0',
            '1', '1', '1',
            '0', '0', '0',
            '0', '0',
            '0', '0', '0', '0',
            '0', '0', '0', '0',
            '0', '0', '0', '0',
            '0', '0', '0', '0', '0', '0', '0',
            '0', '0',
            '0', '0', '0',
            '0', '0', '0');

            DataABTest <= (
            "----------------", "----------------", X"0123", X"0124", X"0123",-- ldi, ldx
            X"FFFF", X"2222", X"5678", -- sts
            "----------------","----------------", X"5555", X"5556", X"5555", -- ldi, ldy 
            X"0000", X"0001", X"0002", -- sts
            "----------------", "----------------", X"EFA0", X"EFA1", X"EFA0", -- ldi, ldz 
            X"0123", X"0123", X"0123", -- stx
            X"5555", X"5594", -- lddy
            X"EFA0", X"EFDF", -- stdz   
            X"EFA0", X"EFDF",-- lddz
            X"5555", X"5594",-- stdy
            "----------------", -- ldi  
            X"ABCD", X"ABCD", X"FFFF",--sts, lds
            "----------------", "----------------", "----------------", -- ldi, mov
            "----------------", "----------------", -- ldi
            X"FFFF", X"0000", X"FFFF", X"FFFE", -- stx
            "----------------", X"5555", X"5560", X"5555", -- ldi, sty
            "----------------", X"EFA0", X"EFA4", X"EFA0", -- ldi, stz
            X"FFFE", X"FFFE", X"5555", X"5555", X"EF9F", X"EF9F", X"EFA0", --ld/stxyz
            "----------------", "----------------", -- ldi
             X"FFFF", X"FFFE",X"FFFD",  -- push
             X"FFFD", X"FFFE", X"FFFF"); -- pop

            DataDBWrTest <= (
            "--------", "--------", "--------", "--------", "--------", -- ldi, ldx
            X"01", X"02", X"01", -- sts
            "--------", "--------", "--------", "--------", "--------", -- ldi, ldy
            X"04", X"05", X"04", -- sts
            "--------", "--------", "--------", "--------", "--------", -- ldi, ldz
            X"07", X"08", X"07", -- stx
            "--------", "--------", -- lddy
            X"04", X"33", -- stdz
            "--------", "--------",-- lddz
            X"07", X"AA",-- stdy
            "--------", -- ldi
            X"88", "--------", "--------", --sts, lds
            "--------", "--------", "--------", -- ldi, mov
            "--------", "--------", -- ldi
            X"88", X"88", X"EE", X"EE", -- stx
            "--------", X"00", X"00", X"00", -- ldi, sty
            "--------", X"56", X"56", X"56", -- ldi, stz
            "--------", X"56", "--------", X"56", "--------", X"9F", X"01",--ld/stxyz
            "--------", "--------", -- ldi
            X"FF", X"56", X"11",  -- push
            "--------", "--------", "--------"); -- pop

            DataDBRdTest <= (
            "--------", "--------", X"01", X"02", X"01",-- ldi, ldx
            "--------", "--------", "--------", -- sts
            "--------", "--------", X"04", X"05", X"04", -- ldi, ldy
            "--------", "--------", "--------", -- sts
            "--------", "--------", X"07", X"08", X"07", -- ldi, ldz
            "--------", "--------", "--------", -- stx
            X"04", X"33", -- lddy
            "--------", "--------", -- stdz
            X"07", X"AA",-- lddz
            "--------", "--------",-- stdy
            "--------", -- ldi
            "--------", X"88", X"01", --sts, lds
            "--------", "--------", "--------", -- ldi, mov
            "--------", "--------",-- ldi
            "--------", "--------", "--------", "--------", -- stx
            "--------", "--------", "--------", "--------", -- ldi, sty
            "--------", "--------", "--------", "--------", -- ldi, stz
            X"EE", "--------", X"56", "--------", X"AA", "--------", "--------", --ld/stxyz
            "--------", "--------", -- ldi
            "--------", "--------", "--------",  -- push
            X"11", X"56", X"FF"); -- pop

            DataRdCheck <= (
            '0', '0', '1' , '1', '1',
            '0', '0', '0',
            '0', '0', '1' , '1', '1',
            '0', '0', '0',
            '0', '0', '1' , '1', '1',
            '0', '0', '0',
            '1', '1',
            '0', '0',
            '1', '1',
            '0', '0',
            '0',
            '0', '1', '1',
            '0', '0', '0',
            '0', '0',
            '0', '0', '0', '0',
            '0', '0', '0', '0',
            '0', '0', '0', '0',
            '1', '0', '1', '0', '1', '0', '0',
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
            '1', '1',
            '1', '1',
            '1', '1',
            '1', '1',
            '0',
            '1', '1', '1',
            '0', '0', '0',
            '0', '0',
            '1', '1', '1', '1',
            '0', '1', '1', '1',
            '0', '1', '1', '1',
            '1', '1', '1', '1', '1', '1', '1',
            '0', '0',
            '1', '1', '1',
            '1', '1', '1');

        	-- initially everything is 0, have not started
            Reset <= '1'; -- begin with reset
			IR      <= (others => '0');
            ProgDB  <= (others => 'Z');
        	wait for CLK_PERIOD*5; -- wait for a bit

            Reset <= '0'; -- de-assert reset
            wait for CLK_PERIOD*0.7; -- offset for clock edge

			-- loop through test vector
			for i in TEST_SIZE downto 0 loop
				IR <= IRTest(i);
                ProgDB <= (others => 'Z');
                WriteToDataDB <= '0';
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
						report  "DataAB failure at test number " & integer'image(TEST_SIZE-i)
						severity  ERROR;

                    -- wait to check data db/ab
                    wait for CLK_PERIOD/2;
                    if DataRdCheck(i) = '1' then
                        WriteToDataDB <= '1';
                        DataData <= DataDBRdTest(i); -- input to data db
                        -- make sure rd/wr signals correct
                        -- check read asserted
                        assert (DataRd = '0')
    						report  "DataRd DataDBRd failure at test number " & integer'image(TEST_SIZE-i)
    						severity  ERROR;
                        -- check write not asserted
                        assert (DataWr = '1')
    						report  "DataWr DataDBRd at test number " & integer'image(TEST_SIZE-i)
    						severity  ERROR;
                    else
                        -- check data db output
                        assert (DataDBWrTest(i) = DataDB)
    						report  "DataDB failure at test number " & integer'image(TEST_SIZE-i)
    						severity  ERROR;
                        -- make sure rd/wr signals correct
                        -- check read not asserted
                        assert (DataRd = '1')
    						report  "DataRd DataDBWr failure at test number " & integer'image(TEST_SIZE-i)
    						severity  ERROR;
                        -- check write asserted
                        assert (DataWr = '0')
    						report  "DataWr DataDBWr at test number " & integer'image(TEST_SIZE-i)
    						severity  ERROR;
                    end if;
                    wait for CLK_PERIOD/2; -- wait for rest of clock
                end if;
			end loop;

            END_SIM <= TRUE;        -- end of stimulus events
            wait;                   -- wait for simulation to end
        end process;

        -- hi-z unless writing to inout dataDB
        dataDB <= DataData when WriteToDataDB = '1' else (others => 'Z');

        -- process for generating system clock
        CLOCK_CLK : process
        begin
            -- this process generates a CLK_PERIOD ns 50% duty cycle clock
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
