----------------------------------------------------------------------------
--
--  Test Bench for AVR Registers
--
--  This is a test bench for the AVR register set.  The test bench thoroughly
--  exercises the register set and register decoding by sending various
--  instructions and then checking that the proper registers are read and
--  written.  By first loading registers with unique values via the
--  instructions and then accessing those registers, the register set is
--  tested.  The test bench entity is called reg_test_tb and it is currently
--  defined to test the REG_TEST entity.
--
--  Revision History:
--     5/18/98   Automated/Active-VHDL    Initial Revision.
--     5/29/98   Glen George              Modified to add documentation and
--                                        more extensive testing.
--     4/24/00   Glen George              Updated comments and modified when
--                                        RegIn is generated.
--     5/11/00   Glen George              Fixed std_match check.
--     1/29/06   Glen George              Updated comments.
--     2/10/13   Glen George              Improved error reporting.
--
----------------------------------------------------------------------------


library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;
--use ieee.std_logic_arith.all;

entity reg_test_tb is
end reg_test_tb;


architecture TB_ARCHITECTURE of reg_test_tb is

    subtype  opcode_word  is  std_logic_vector(15 downto 0);

    -- Component declaration of the tested unit
    component REG_TEST
        port (
            IR       :  in   opcode_word;
            RegIn    :  in   std_logic_vector(7 downto 0);
            clock    :  in   std_logic;
            RegAOut  :  out  std_logic_vector(7 downto 0);
            RegBOut  :  out  std_logic_vector(7 downto 0)
        );
    end component;


    -- Stimulus signals - signals mapped to the input and inout ports of tested entity
    signal  IR     :  opcode_word;
    signal  RegIn  :  std_logic_vector(7 downto 0);
    signal  clock  :  std_logic;

    -- Observed signals - signals mapped to the output ports of tested entity
    signal  RegAOut  :  std_logic_vector(7 downto 0);
    signal  RegBOut  :  std_logic_vector(7 downto 0);

    --Signal used to stop clock signal generators
    signal  END_SIM  :  BOOLEAN := FALSE;

    -- test value types
    type  opcode_array  is array (natural range <>) of opcode_word;
    type  byte_array    is array (natural range <>) of std_logic_vector(7 downto 0);
    type  inst_array    is array (natural range <>) of string(1 to 13);

    -- actual test vectors

    -- the instructions
    constant  TestInstructions : inst_array(0 to 109) := (
        "SBCI R25,  R7", "ASR   R3, R13", "SBCI R18, R28", "OR   R23, R25",
        "NEG   R6, R26", "ASR   R2, R11", "ADD  R20,  R2", "NEG   R1, R15",
        "ASR  R12, R19", "ADC  R26,  R6", "OR   R19, R23", "SBC  R27,  R3",
        "ROR   R4, R31", "ADD  R22, R18", "ASR   R7,  R9", "SUB  R16,  R4",
        "ASR   R9,  R5", "SUBI R24, R29", "SUB  R13, R20", "SBC  R29, R16",
        "ADD  R15, R24", "ORI  R17, R10", "SUBI R31, R30", "AND  R30,  R1",
        "OR   R28, R17", "SWAP  R0, R14", "ADC  R14, R22", "BLD   R8,  R8",
        "ADD  R21, R27", "ADC  R10,  R0", "AND   R5, R12", "ADD  R11, R21",
        "SUB   R1,  R3", "SUB  R12,  R6", "SBC   R2, R11", "ADD  R28, R23",
        "EOR  R13, R21", "SUB  R10,  R4", "OR   R30, R29", "ADC  R29, R19",
        "EOR   R0, R15", "ADC  R23, R10", "SBC  R15, R12", "SUB  R31,  R8",
        "EOR  R26, R27", "ADC  R21, R13", "EOR   R6, R26", "ADC  R24,  R1",
        "ADC   R7, R22", "OR   R20,  R9", "SBC   R4, R25", "EOR   R5,  R2",
        "SBC  R18,  R5", "SUB  R11, R31", "AND   R9, R17", "AND  R17, R14",
        "SUB  R25, R20", "AND  R22, R28", "OR   R16,  R7", "EOR  R14, R18",
        "SUB   R8, R16", "EOR   R3, R30", "ADC  R19, R24", "OR   R27,  R0",
        "EOR  R15, R25", "SBC  R26, R20", "AND  R21, R17", "ADD  R22, R22",
        "SBC  R19, R26", "SBC   R9, R10", "EOR   R7,  R4", "SUB   R2, R27",
        "OR   R14, R23", "EOR   R4, R19", "ADD  R30, R12", "SUB  R28, R11",
        "OR   R17,  R2", "EOR  R27, R16", "ADC   R6, R24", "ADD  R24, R31",
        "SBC  R12, R13", "SBC   R0,  R1", "ADD   R1, R28", "OR   R11, R30",
        "SBC  R31,  R5", "OR   R18,  R8", "EOR  R13, R29", "ADD  R29,  R9",
        "ADC   R8, R18", "EOR   R3, R15", "AND  R20,  R6", "EOR  R16, R21",
        "ADC  R23, R14", "AND  R25,  R3", "ADD  R10,  R0", "OR    R5,  R7",
        "CP   R25,  R5", "CPC  R27, R25", "CPC  R30, R27", "BST   R7, R30",
        "CPI  R22,  R7", "CPC  R10, R22", "CPI  R27, R10", "CPC  R16, R27",
        "CP   R24, R16", "CP    R8, R24", "CPI  R21,  R8", "BST   R5, R21",
        "BST  R10,  R5", "CPI  R20, R10"                                    );

    -- the Instruction Register values
    signal IRTestVals : opcode_array(0 to 109) := (
        "0100000010010000", "1001010000110101", "0100000000100000",
        "0010101101111001", "1001010001100001", "1001010000100101",
        "0000110101000010", "1001010000010001", "1001010011000101",
        "0001110110100110", "0010101100110111", "0000100110110011",
        "1001010001000111", "0000111101100010", "1001010001110101",
        "0001100100000100", "1001010010010101", "0101000010000000",
        "0001101011010100", "0000101111010000", "0000111011111000",
        "0110000000010000", "0101000011110000", "0010000111100001",
        "0010101111000001", "1001010000000010", "0001111011100110",
        "1111100010000000", "0000111101011011", "0001110010100000",
        "0010000001011100", "0000111010110101", "0001100000010011",
        "0001100011000110", "0000100000101011", "0000111111000111",
        "0010011011010101", "0001100010100100", "0010101111101101",
        "0001111111010011", "0010010000001111", "0001110101111010",
        "0000100011111100", "0001100111111000", "0010011110101011",
        "0001110101011101", "0010011001101010", "0001110110000001",
        "0001111001110110", "0010100101001001", "0000101001001001",
        "0010010001010010", "0000100100100101", "0001101010111111",
        "0010001010010001", "0010000100011110", "0001101110010100",
        "0010001101101100", "0010100100000111", "0010011011100010",
        "0001101010000000", "0010011000111110", "0001111100111000",
        "0010100110110000", "0010011011111001", "0000101110100100",
        "0010001101010001", "0000111101100110", "0000101100111010",
        "0000100010011010", "0010010001110100", "0001101000101011",
        "0010101011100111", "0010011001000011", "0000110111101100",
        "0001100111001011", "0010100100010010", "0010011110110000",
        "0001111001101000", "0000111110001111", "0000100011001101",
        "0000100000000001", "0000111000011100", "0010101010111110",
        "0000100111110101", "0010100100101000", "0010011011011101",
        "0000110111011001", "0001111010000010", "0010010000111111",
        "0010000101000110", "0010011100000101", "0001110101111110",
        "0010000110010011", "0000110010100000", "0010100001010111",
        "0001010110010101", "0000011110111001", "0000011111101011",
        "1111101001110000", "0011000001100000", "0000011010100110",
        "0011000010110000", "0000011100001011", "0001011110000000",
        "0001011010001000", "0011000001010000", "1111101001010000",
        "1111101010100000", "0011000001000000"                      );

    -- input register values for each instruction
    signal RegInVals  : byte_array(0 to 109) := (
        "00110011", "10000010", "10011101", "10101001", "11100001",
        "11000010", "10101011", "11101101", "01111110", "00100010",
        "11010100", "00001000", "01100000", "10100101", "01100111",
        "00110110", "01101010", "11010101", "10011110", "10110000",
        "11011010", "00000001", "00001011", "00001011", "01001111",
        "10111100", "01000000", "11010011", "00011001", "10010011",
        "10111110", "01101000", "00100000", "10110110", "10110101",
        "00101111", "01111001", "01101110", "00011010", "10111110",
        "11000101", "10100011", "00010100", "10111011", "10011110",
        "00000010", "00100100", "10111011", "10001001", "10011111",
        "00010111", "00101111", "11000100", "00110011", "01110000",
        "11001001", "01010010", "10011011", "00010011", "00001110",
        "01000101", "10001101", "00110100", "00111000", "11000110",
        "11111111", "10110010", "11001100", "01000100", "10100110",
        "00010000", "01100110", "11001011", "01111000", "00001110",
        "11010100", "00110110", "11001010", "01011110", "00000011",
        "10101100", "00111011", "01100000", "10100010", "01101110",
        "11101110", "10001000", "01111010", "11100000", "00001001",
        "00001101", "00001011", "11010110", "01101110", "11010100",
        "01101000", "10011100", "01111010", "11101111", "10111010",
        "11011001", "01111000", "01111101", "00101010", "01110110",
        "10110000", "10001001", "10101011", "00010110", "00010011"  );

    -- expected Register A output for each instruction
    signal RegACmps   : byte_array(0 to 109) := (
        "--------", "--------", "--------", "--------", "--------",
        "--------", "--------", "--------", "--------", "--------",
        "--------", "--------", "--------", "--------", "--------",
        "--------", "--------", "--------", "--------", "--------",
        "--------", "--------", "--------", "--------", "--------",
        "--------", "--------", "--------", "--------", "--------",
        "--------", "--------", "11101101", "01111110", "11000010",
        "01001111", "10011110", "10010011", "00001011", "10110000",
        "10111100", "10101001", "11011010", "00001011", "00100010",
        "00011001", "11100001", "11010101", "01100111", "10101011",
        "01100000", "10111110", "10011101", "01101000", "01101010",
        "00000001", "00110011", "10100101", "00110110", "01000000",
        "11010011", "10000010", "11010100", "00001000", "00010100",
        "10011110", "00000010", "10011011", "00110100", "01110000",
        "10001001", "10110101", "00001110", "00010111", "00011010",
        "00101111", "11001001", "00111000", "00100100", "10111011",
        "10110110", "11000101", "00100000", "00110011", "10111011",
        "11000100", "01111001", "10111110", "01000101", "10001101",
        "10011111", "00010011", "10100011", "01010010", "01101110",
        "00101111", "01101110", "11001010", "00001110", "00010000",
        "11001100", "11010100", "11001010", "00001011", "00000011",
        "11100000", "10110010", "01101000", "11010100", "00001101"  );

    -- expected Register B output for each instruction
    signal RegBCmps   : byte_array(0 to 109) := (
        "--------", "--------", "--------", "00110011", "--------",
        "--------", "11000010", "--------", "--------", "11100001",
        "10101001", "10000010", "--------", "10011101", "--------",
        "01100000", "--------", "--------", "10101011", "00110110",
        "11010101", "--------", "--------", "11101101", "00000001",
        "--------", "10100101", "--------", "00001000", "10111100",
        "01111110", "00011001", "10000010", "11100001", "01101000",
        "10101001", "00011001", "01100000", "10110000", "11010100",
        "11011010", "01101110", "10110110", "11010011", "00001000",
        "01111001", "10011110", "00100000", "10100101", "01101010",
        "00110011", "10110101", "00101111", "10111011", "00000001",
        "01000000", "10011111", "00101111", "10001001", "11000100",
        "00010011", "00011010", "10111011", "11000101", "01010010",
        "10011111", "11001001", "10011011", "11111111", "01101110",
        "00010111", "00111000", "10100011", "01000100", "10110110",
        "00110011", "01100110", "00010011", "10111011", "10111011",
        "01111001", "00100000", "11010100", "00001110", "00101111",
        "01000101", "10111110", "10100110", "11101110", "11000110",
        "01011110", "10110010", "11001011", "00001001", "00111011",
        "00010000", "01101000", "01101110", "11001010", "--------",
        "--------", "11001100", "--------", "11001010", "00001011",
        "00000011", "--------", "--------", "--------", "--------"  );


begin

    -- Unit Under Test port map
    UUT : REG_TEST
        port map  (
            IR      => IR,
            RegIn   => RegIn,
            clock   => clock,
            RegAOut => RegAOut,
            RegBOut => RegBOut
        );


    -- now generate the stimulus and test the design
    process
    begin

        -- set the inputs
        IR <= IRTestVals(0);
        RegIn <= RegInVals(0);
        -- wait for everything to get synced up
        wait for 270 ns;

        -- now loop on the test values
        for test_no in RegInVals'range  loop
        
            -- generate the instruction and register input
            if  ((test_no) > IRTestVals'high)  then
                -- out of test data, use last data
                IR <= IRTestVals(IRTestVals'high);
            else
                -- still have test data, use it
                IR <= IRTestVals(test_no);
            end if;

            -- wait for the outputs to be ready
            wait for 60 ns;

            -- test the outputs
            if  (std_match(RegAOut, RegACmps(test_no)))  then
                -- nothing wrong - be silent
            else
                -- there is an error on RegA - report it
                REPORT "Failed Test #" & integer'image(test_no) &
                       " - RegAOut Failure --" &
                       " Instruction: " & TestInstructions(test_no) &
                       " IR: " & integer'image(to_integer(unsigned(IR))) &
                       " RegA: " & integer'image(to_integer(unsigned(RegAOut))) &
                       " Expected RegA: " & integer'image(to_integer(unsigned(RegACmps(test_no))))
                SEVERITY ERROR;
            end if;
            if  (std_match(RegBOut, RegBCmps(test_no)))  then
                -- match is OK - nothing to report
            else
                -- there is an error on RegB - report it
                REPORT "Failed Test #" & integer'image(test_no) &
                       " - RegBOut Failure --" &
                       " Instruction: " & TestInstructions(test_no) &
                       " IR: " & integer'image(to_integer(unsigned(IR))) &
                       " RegB: " & integer'image(to_integer(unsigned(RegBOut))) &
                       " Expected RegB: " & integer'image(to_integer(unsigned(RegBCmps(test_no))))
                SEVERITY ERROR;
            end if;


            -- generate the input to the register array too
            RegIn <= RegInVals(test_no);


            -- now wait for the next clock
            wait for 40 ns;

        end loop;


        END_SIM <= TRUE;

        --  end of stimulus events, wait for simulation to end
        wait;

    end process; -- end of stimulus process
    

    -- clock process
    --
    -- generates a clock with a 100 ns period and a 50% duty cycle

    CLOCK_clock : process
    begin

        -- stop the clock when the end of the test vectors is reached
        if  END_SIM = FALSE  then
            clock <= '0';
            wait for 50 ns;
        else
            wait;
        end if;

        if  END_SIM = FALSE  then
            clock <= '1';
            wait for 50 ns;
        else
            wait;
        end if;

    end process;


end TB_ARCHITECTURE;
