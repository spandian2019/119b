----------------------------------------------------------------------------
--
--  Test Bench for AVR ALU
--
--  This is a test bench for the AVR ALU.  The test bench thoroughly
--  exercises the ALU by sending various operations and then checking both
--  the ALU result and the status flags.  The status flags are tested
--  individually to make testing a little easier.  The test bench entity is
--  called alu_test_tb and it is currently defined to test the ALU_TEST
--  entity.
--
--  Revision History:
--     4/16/98   Automated/Active-VHDL    Initial Revision.
--     5/11/00   Glen George              Added much more extensive testing.
--     6/6/04    Glen George              Updated test vectors.
--     6/6/04    Glen George              Added better flag testing.
--     1/29/06   Glen George              Updated comments.
--     1/30/10   Glen George              Changed the test bench so the same
--                                        value is on OperandA and OperandB
--                                        for single operand instructions.
--     1/29/12   Glen George              Changed the test bench so the bit
--                                        number is on OperandB for bit
--                                        operation instructions.
--     2/10/13   Glen George              Changed flag comparisons to
--                                        specifically check for 'U' values
--                                        since assigning 'U' (to store old
--                                        value) changes it 'X'.
--     2/10/13   Glen George              Improved testing of CPC instruction.
--     2/10/13   Glen George              Improved error reporting.
--     2/11/13   Glen George              Fixed some syntax errors.
--     2/16/13   Glen George              Fixed the status register error
--                                        reporting (the instruction reported
--                                        was off by one).
--
----------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
--use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;


entity alu_test_tb is
end alu_test_tb;


architecture TB_ARCHITECTURE of alu_test_tb is

    subtype  opcode_word  is  std_logic_vector(15 downto 0);

    -- Component declaration of the tested unit
    component ALU_TEST
        port (
            IR        :  in   opcode_word;                   -- instruction register
            OperandA  :  in   std_logic_vector(7 downto 0);  -- first operand
            OperandB  :  in   std_logic_vector(7 downto 0);  -- second operand
            clock     :  in   std_logic;                     -- system clock
            Result    :  out  std_logic_vector(7 downto 0);  -- ALU result
            StatReg   :  out  std_logic_vector(7 downto 0)   -- status register
        );
    end component;


    -- Stimulus signals - signals mapped to the input and inout ports of tested entity
    signal  Clock     :  std_logic;
    signal  OperandA  :  std_logic_vector(7 downto 0);
    signal  OperandB  :  std_logic_vector(7 downto 0);
    signal  IR        :  opcode_word;

    -- Observed signals - signals mapped to the output ports of tested entity
    signal  Result    :  std_logic_vector(7 downto 0);
    signal  StatReg   :  std_logic_vector(7 downto 0);

    --Signal used to stop clock signal generators
    signal  END_SIM   :  BOOLEAN:=FALSE;

    -- previous value of status register for testing flag register changes
    signal  PrevStatReg  :  std_logic_vector(7 downto 0);

    -- flag bit names
    constant  StatRegNames   :  string(8 downto 1) := "ITHSVNZC";

    -- test value types
    type  opcode_array  is array (natural range <>) of opcode_word;
    type  byte_array    is array (natural range <>) of std_logic_vector(7 downto 0);
    type  inst_array    is array (natural range <>) of string(1 to 14);

    -- actual test vectors

    -- the instructions
    constant  TestInstructions : inst_array(0 to 139) := (
        "BCLR  0       ", "BCLR  7       ", "BCLR  4       ",
        "BCLR  3       ", "BCLR  1       ", "BCLR  5       ",
        "BCLR  2       ", "BCLR  6       ", "BSET  4       ",
        "BSET  1       ", "BSET  7       ", "BSET  0       ",
        "BSET  2       ", "BSET  6       ", "BSET  5       ",
        "BSET  3       ", "BLD   R21, 7  ", "BLD   R7, 3   ",
        "BLD   R17, 1  ", "BLD   R12, 6  ", "BLD   R1, 0   ",
        "BLD   R31, 5  ", "BLD   R10, 4  ", "BLD   R27, 2  ",
        "BST   R15, 5  ", "BST   R23, 2  ", "BST   R13, 7  ",
        "BST   R2, 0   ", "BST   R22, 1  ", "BST   R11, 6  ",
        "BST   R3, 3   ", "BST   R15, 4  ", "ADC   R1, R5  ",
        "ADC   R6, R9  ", "ADC   R17, R14", "ADC   R20, R10",
        "ADC   R20, R10", "ADD   R4, R30 ", "ADD   R19, R25",
        "ADD   R21, R18", "ADD   R16, R8 ", "ADIW  R24, $0 ",
        "ADIW  R24, $0 ", "ADIW  R26, $10", "ADIW  R26, $10",
        "ADIW  R28, $3F", "ADIW  R28, $3F", "ADIW  R30, $27",
        "ADIW  R30, $27", "AND   R13, R4 ", "AND   R19, R2 ",
        "AND   R0, R31 ", "ANDI  R17, $FF", "ANDI  R22, $FF",
        "ANDI  R27, $FF", "ASR   R8      ", "ASR   R20     ",
        "ASR   R3      ", "ASR   R28     ", "COM   R4      ",
        "COM   R24     ", "COM   R26     ", "COM   R16     ",
        "CP    R31, R21", "CP    R1, R15 ", "CP    R5, R12 ",
        "CPC   R15, R17", "CPC   R22, R27", "CPC   R11, R4 ",
        "CPC   R12, R26", "CPC   R24, R29", "CPC   R6, R2  ",
        "CPC   R12, R15", "CPC   R13, R15", "CPC   R12, R15",
        "CP    R24, R11", "CPC   R29, R17",
        "CPI   R24, $7F", "CPI   R16, $70", "CPI   R19, $A0",
        "DEC   R6      ", "DEC   R23     ", "DEC   R12     ",
        "DEC   R9      ", "EOR   R2, R3  ", "EOR   R5, R20 ",
        "EOR   R31, R30", "EOR   R22, R7 ", "EOR   R4, R17 ",
        "INC   R16     ", "INC   R21     ", "INC   R1      ",
        "INC   R19     ", "LSR   R8      ", "LSR   R28     ",
        "LSR   R18     ", "LSR   R0      ", "NEG   R29     ",
        "NEG   R15     ", "NEG   R24     ", "NEG   R5      ",
        "OR    R3, R2  ", "OR    R20, R5 ", "OR    R30, R31",
        "ORI   R16, $FF", "ORI   R17, $5A", "ORI   R17, $83",
        "ROR   R31     ", "ROR   R30     ", "ROR   R29     ",
        "ROR   R28     ", "ROR   R27     ", "SBC   R15, R17",
        "SBCI  R22, $70", "SBC   R11, R4 ", "SBCI  R30, $7F",
        "SBC   R24, R9 ", "SBCI  R26, $A0", "SBIW  R28, $10",
        "SBIW  R28, $10", "SBIW  R26, $0 ", "SBIW  R26, $0 ",
        "SBIW  R24, $17", "SBIW  R24, $17", "SBIW  R28, $31",
        "SBIW  R28, $31", "SBIW  R30, $25", "SBIW  R30, $25",
        "SBIW  R24, $3F", "SBIW  R24, $3F", "SUB   R13, R12",
        "SUB   R10, R25", "SUB   R20, R21", "SUBI  R30, $7F",
        "SUBI  R22, $70", "SUBI  R23, $A0", "SWAP  R11     ",
        "SWAP  R16     ", "SWAP  R3      ", "SWAP  R7      "  );

    -- the Instruction Register values
    signal IRTestVals   :  opcode_array(0 to 139) := (
                               X"9488", X"94F8", X"94C8", X"94B8", X"9498",
                               X"94D8", X"94A8", X"94E8", X"9448", X"9418",
                               X"9478", X"9408", X"9428", X"9468", X"9458",
                               X"9438", X"F957", X"F873", X"F911", X"F8C6",
                               X"F810", X"F9F5", X"F8A4", X"F9B2", X"FAF5",
                               X"FB72", X"FAD7", X"FA20", X"FB61", X"FAB6",
                               X"FA33", X"FAF4", X"1C15", X"1C69", X"1D1E",
                               X"1D4A", X"1D4A", X"0E4E", X"0F39", X"0F52",
                               X"0D08", X"9600", X"9600", X"9650", X"9650",
                               X"96EF", X"96EF", X"96B7", X"96B7", X"20D4",
                               X"2132", X"220F", X"7F1F", X"7F6F", X"7FBF",
                               X"9485", X"9545", X"9435", X"95C5", X"9440",
                               X"9580", X"95A0", X"9500", X"17F5", X"141F",
                               X"145C", X"06F1", X"076B", X"04B4", X"06CA",
                               X"078D", X"0462", X"04CF", X"04DF", X"04CF",
                               X"158b", X"07D1", X"378F", X"3700", X"3A30",
                               X"946A", X"957A", X"94CA", X"949A", X"2423",
                               X"2654", X"27FE", X"2567", X"2641", X"9503",
                               X"9553", X"9413", X"9533", X"9486", X"95C6",
                               X"9526", X"9406", X"95D1", X"94F1", X"9581",
                               X"9451", X"2832", X"2945", X"2BEF", X"6F0F",
                               X"651A", X"6813", X"95F7", X"95E7", X"95D7",
                               X"95C7", X"95B7", X"0AF1", X"4760", X"08B4",
                               X"47EF", X"0989", X"4AA0", X"9760", X"9760",
                               X"9710", X"9710", X"9747", X"9747", X"97E1",
                               X"97E1", X"97B5", X"97B5", X"97CF", X"97CF",
                               X"18DC", X"1AA9", X"1B45", X"57EF", X"5760",
                               X"5A70", X"94B2", X"9502", X"9432", X"9472" );

    -- Operand A values to ALU for each instruction
    signal OpATestVals  :  byte_array(0 to 139) := (
                               "XXXXXXXX", "XXXXXXXX", "XXXXXXXX", "XXXXXXXX", "XXXXXXXX",
                               "XXXXXXXX", "XXXXXXXX", "XXXXXXXX", "XXXXXXXX", "XXXXXXXX",
                               "XXXXXXXX", "XXXXXXXX", "XXXXXXXX", "XXXXXXXX", "XXXXXXXX",
                               "XXXXXXXX", X"00", X"00", X"00", X"00",
                               X"00", X"00", X"00", X"00", X"DF",
                               X"04", X"7F", X"01", X"FD", X"40",
                               X"F7", X"10", X"FF", X"FF", X"00",
                               X"80", X"7E", X"FF", X"00", X"00",
                               X"80", X"7D", X"45", X"F0", X"FF",
                               X"C2", X"7E", X"E9", X"FF", X"55",
                               X"FF", X"00", X"55", X"FF", X"AA",
                               X"FF", X"00", X"70", X"80", X"FF",
                               X"00", X"55", X"AA", X"00", X"7F",
                               X"3F", X"00", X"50", X"7F", X"00",
                               X"7F", X"71", X"00", X"01", X"00",
                               X"7F", X"FF", X"00", X"50", X"71",
                               X"00", X"80", X"70", X"FF", X"55",
                               X"FF", X"00", X"AA", X"5A", X"00",
                               X"FF", X"7F", X"90", X"FF", X"70",
                               X"00", X"80", X"FF", X"00", X"80",
                               X"7F", X"55", X"FF", X"00", X"AA",
                               X"5A", X"70", X"FF", X"70", X"01",
                               X"80", X"00", X"00", X"50", X"7F",
                               X"00", X"7F", X"71", X"0D", X"00",
                               X"F0", X"FF", X"17", X"00", X"E0",
                               X"FF", X"C2", X"7F", X"00", X"FF",
                               X"00", X"7F", X"7F", X"00", X"50",
                               X"71", X"5A", X"67", X"9F", X"00" );

    -- Operand B values to ALU for each instruction
    signal OpBTestVals  :  byte_array(0 to 139) := (
                               X"00", X"07", X"04", X"03", X"01",
                               X"05", X"02", X"06", X"04", X"01",
                               X"07", X"00", X"02", X"06", X"05",
                               X"03", X"07", X"03", X"01", X"06",
                               X"00", X"05", X"04", X"02", X"05",
                               X"02", X"07", X"00", X"01", X"06",
                               X"03", X"04", X"FF", X"00", X"FF",
                               X"7E", X"80", X"7E", X"FF", X"00",
                               X"80", X"00", "XXXXXXXX", X"10", "XXXXXXXX",
                               X"3F", "XXXXXXXX", X"27", "XXXXXXXX", X"AA",
                               X"55", X"AA", X"FF", X"FF", X"FF",
                               X"FF", X"00", X"70", X"80", X"FF",
                               X"00", X"55", X"AA", X"FF", X"FF",
                               X"40", X"FF", X"70", X"00", X"7F",
                               X"FF", X"A0", X"00", X"00", X"00",
                               X"7F", X"FF", X"7F", X"70", X"A0",
                               X"00", X"80", X"70", X"FF", X"AA",
                               X"55", X"AA", X"FF", X"5A", X"00",
                               X"FF", X"7F", X"90", X"FF", X"70",
                               X"00", X"80", X"FF", X"00", X"80",
                               X"7F", X"AA", X"55", X"AA", X"FF",
                               X"5A", X"83", X"FF", X"70", X"01",
                               X"80", X"00", X"FF", X"70", X"00",
                               X"7F", X"FF", X"A0", X"10", "XXXXXXXX",
                               X"00", "XXXXXXXX", X"17", "XXXXXXXX", X"31",
                               "XXXXXXXX", X"25", "XXXXXXXX", X"3F", "XXXXXXXX",
                               X"FF", X"FF", X"00", X"7F", X"70",
                               X"A0", X"5A", X"67", X"9F", X"00" );

    -- expected ALU result for each instruction
    signal ResultCmps   :  byte_array(0 to 139) := (
                               "--------", "--------", "--------", "--------", "--------",
                               "--------", "--------", "--------", "--------", "--------",
                               "--------", "--------", "--------", "--------", "--------",
                               "--------", X"80", X"08", X"02", X"40",
                               X"01", X"20", X"10", X"04", "--------",
                               "--------", "--------", "--------", "--------", "--------",
                               "--------", "--------", X"FF", X"00", X"00",
                               X"FF", X"FE", X"7D", X"FF", X"00",
                               X"00", X"7D", X"45", X"00", X"00",
                               X"01", X"7F", X"10", X"00", X"00",
                               X"55", X"00", X"55", X"FF", X"AA",
                               X"FF", X"00", X"38", X"C0", X"00",
                               X"FF", X"AA", X"55", "--------", "--------",
                               "--------", "--------", "--------", "--------", "--------",
                               "--------", "--------", "--------", "--------", "--------",
                               "--------", "--------", "--------", "--------", "--------",
                               X"FF", X"7F", X"6F", X"FE", X"FF",
                               X"AA", X"AA", X"55", X"00", X"01",
                               X"00", X"80", X"91", X"7F", X"38",
                               X"00", X"40", X"01", X"00", X"80",
                               X"81", X"FF", X"FF", X"AA", X"FF",
                               X"5A", X"F3", X"FF", X"B8", X"00",
                               X"C0", X"00", X"01", X"DF", X"7E",
                               X"81", X"7F", X"D0", X"FD", X"FF",
                               X"F0", X"FF", X"00", X"00", X"AF",
                               X"FF", X"9D", X"7F", X"C1", X"FE",
                               X"01", X"80", X"7F", X"81", X"E0",
                               X"D1", X"A5", X"76", X"F9", X"00" );

    -- expected status register results for each instruction
    signal StatRegCmps  :  byte_array(0 to 139) := (
                               "-------0", "0------0", "0--0---0", "0--00--0", "0--00-00",
                               "0-000-00", "0-000000", "00000000", "00010000", "00010010",
                               "10010010", "10010011", "10010111", "11010111", "11110111",
                               "11111111", "11111111", "11111111", "11111111", "11111111",
                               "11111111", "11111111", "11111111", "11111111", "10111111",
                               "11111111", "10111111", "11111111", "10111111", "11111111",
                               "10111111", "11111111", "11110101", "11100011", "11100011",
                               "11010100", "11010100", "11100001", "11010100", "11000010",
                               "11011011", "--------", "11000000", "--------", "11000011",
                               "--------", "11000000", "--------", "11000001", "11000011",
                               "11000001", "11000011", "11000001", "11010101", "11010101",
                               "11010101", "11000010", "11000000", "11001100", "11000011",
                               "11010101", "11010101", "11000001", "11100001", "11001101",
                               "11010101", "11100001", "11110101", "11000000", "11110101",
                               "11100001", "11001101", "11110101", "11000000", "11000000",
                               "11000010", "11000010", "11110101", "11010101", "11001101",
                               "11010101", "11011001", "11000001", "11010101", "11010101",
                               "11010101", "11010101", "11000001", "11000011", "11000001",
                               "11000011", "11001101", "11010101", "11011001", "11000000",
                               "11000010", "11000000", "11100001", "11000010", "11001101",
                               "11110101", "11110101", "11110101", "11110101", "11110101",
                               "11100001", "11110101", "11110101", "11101100", "11111011",
                               "11101100", "11100010", "11100001", "11110101", "11000000",
                               "11110101", "11100001", "11001101", "--------", "11010101",
                               "--------", "11010100", "--------", "11000010", "--------",
                               "11010100", "--------", "11000000", "--------", "11010100",
                               "11100001", "11001101", "11000000", "11110101", "11010101",
                               "11001101", "11001101", "11001101", "11001101", "11001101" );

    -- whether or not each status register bit changes on each instruction
    signal StatRegMask  :  byte_array(0 to 139) := (
                               "00000001", "10000000", "00010000", "00001000", "00000010",
                               "00100000", "00000100", "01000000", "00010000", "00000010",
                               "10000000", "00000001", "00000100", "01000000", "00100000",
                               "00001000", "00000000", "00000000", "00000000", "00000000",
                               "00000000", "00000000", "00000000", "00000000", "01000000",
                               "01000000", "01000000", "01000000", "01000000", "01000000",
                               "01000000", "01000000", "00111111", "00111111", "00111111",
                               "00111111", "00111111", "00111111", "00111111", "00111111",
                               "00111111", "00011111", "00011111", "00011111", "00011111",
                               "00011111", "00011111", "00011111", "00011111", "00011110",
                               "00011110", "00011110", "00011110", "00011110", "00011110",
                               "00011111", "00011111", "00011111", "00011111", "00011111",
                               "00011111", "00011111", "00011111", "00111111", "00111111",
                               "00111111", "00111111", "00111111", "00111111", "00111111",
                               "00111111", "00111111", "00111111", "00111111", "00111111",
                               "00111111", "00111111", "00111111", "00111111", "00111111",
                               "00011110", "00011110", "00011110", "00011110", "00011110",
                               "00011110", "00011110", "00011110", "00011110", "00011110",
                               "00011110", "00011110", "00011110", "00011111", "00011111",
                               "00011111", "00011111", "00111111", "00111111", "00111111",
                               "00111111", "00011110", "00011110", "00011110", "00011110",
                               "00011110", "00011110", "00011111", "00011111", "00011111",
                               "00011111", "00011111", "00111111", "00111111", "00111111",
                               "00111111", "00111111", "00111111", "00011111", "00011111",
                               "00011111", "00011111", "00011111", "00011111", "00011111",
                               "00011111", "00011111", "00011111", "00011111", "00011111",
                               "00111111", "00111111", "00111111", "00111111", "00111111",
                               "00111111", "00000000", "00000000", "00000000", "00000000" );


begin

    -- Unit Under Test port map
    UUT : ALU_TEST
        port map  (
            Clock     =>  Clock,
            Result    =>  Result,
            StatReg   =>  StatReg,
            OperandA  =>  OperandA,
            OperandB  =>  OperandB,
            IR        =>  IR
        );


    -- now generate the stimulus and test the design
    process


        -- flag errors
        variable  StatRegErr     :  string(8 downto 1);
        variable  StatRegChgErr  :  string(8 downto 1);


    begin

        -- set the inputs
        IR <= IRTestVals(0);
        OperandA <= OpATestVals(0);
        OperandB <= OpBTestVals(0);

        -- wait for everything to get synced up
        wait for 270 ns;

	-- get the previous value of the status register
	PrevStatReg <= StatReg;


        -- now loop on the test values
        for test_no in IRTestVals'range  loop

            -- generate the instruction and operand input

	    -- the input instruction register value
            if  (test_no > IRTestVals'high)  then
                -- out of test data, use last data
                IR <= IRTestVals(IRTestVals'high);
            else
                -- still have test data, use it
                -- if there is a timing problem may need to add a "+ 1"
                IR <= IRTestVals(test_no);
            end if;

	    -- the Operand A input to the ALU
            if  (test_no > OpATestVals'high)  then
                -- out of test data, use last data
                OperandA <= OpATestVals(OpATestVals'high);
            else
                -- still have test data, use it
                -- if there is a timing problem may need to add a "+ 1"
                OperandA <= OpATestVals(test_no);
            end if;

	    -- the Operand B input to the ALU
            if  (test_no > OpBTestVals'high)  then
                -- out of test data, use last data
                OperandB <= OpBTestVals(OpBTestVals'high);
            else
                -- still have test data, use it
                -- if there is a timing problem may need to add a "+ 1"
                OperandB <= OpBTestVals(test_no);
            end if;


            -- wait for the outputs to be ready
            wait for 60 ns;


            -- test the outputs (if any)

	    -- check the ALU result
            if  (test_no <= ResultCmps'high)  then
                if  (std_match(Result, ResultCmps(test_no)))  then
                    -- nothing to report - all is well
                else
                    -- there is an error on Result - report it
		    REPORT "Failed Test #" & integer'image(test_no) &
                           " - Result Failure -- " &
                           " Instruction: " & TestInstructions(test_no) &
                           " IR: " & integer'image(to_integer(unsigned(IR))) &
                           " OperandA: " & integer'image(to_integer(unsigned(OperandA))) &
                           " OperandB: " & integer'image(to_integer(unsigned(OperandB))) &
                           " Result: " & integer'image(to_integer(unsigned(Result))) &
                           " Expected Result: " & integer'image(to_integer(unsigned(ResultCmps(test_no))))
                    SEVERITY ERROR;
                end if;
            end if;

	    -- no status register errors yet
	    StatRegErr := "--------";
	    StatRegChgErr := "--------";

	    -- check the status register
            if  (((test_no - 1) >= StatRegCmps'low) and
                 ((test_no - 1) <= StatRegCmps'high))  then
		-- actually check the status register bit by bit

		-- now loop on each bit
		for bit_no in StatReg'range  loop

		    -- first check if the bit should change
		    if  (StatRegMask(test_no - 1)(bit_no) = '1')  then
		        -- bit is changing - see that it does
                        if  (std_match(StatReg(bit_no), StatRegCmps(test_no - 1)(bit_no)))  then
                            -- good results, don't need to report anything
                        else
                            -- there is an error on the Status Register
                            --    set error (note string index is 1-based)
                            StatRegErr(bit_no + 1) := StatRegNames(bit_no + 1);
                        end if;
		    else
		        -- bit is not changing - make sure it doesn't
		        --   note that std_match doesn't seem to work with 'U'
                        if  (std_match(StatReg(bit_no), PrevStatReg(bit_no)) or
                             ((StatReg(bit_no) = 'U') and (PrevStatReg(bit_no) = 'U')))  then
                            -- good results, don't need to report anything
                        else
                            -- there is an error on the Status Register
                            --    set error (note string index is 1-based)
                            StatRegChgErr(bit_no + 1) := StatRegNames(bit_no + 1);
                        end if;
                    end if;
                end loop;
            end if;

            -- check if there were status register errors
            -- remember status register failures are on the previous
            --    instruction since updates are delayed by one cycle
	    if (StatRegErr /= "--------")  then
		REPORT "Failed Test #" & integer'image(test_no - 1) &
                       " - Status Register Value Failure -- " &
                       " Instruction: " & TestInstructions(test_no - 1) &
                       " IR: " & integer'image(to_integer(unsigned((IRTestVals(test_no - 1))))) &
                       " Operand A: " & integer'image(to_integer(unsigned(OpATestVals(test_no - 1)))) &
                       " Operand B: " & integer'image(to_integer(unsigned(OpBTestVals(test_no - 1)))) &
                       " Status Reg: " & integer'image(to_integer(unsigned(StatReg))) &
                       " Expected Status Reg: " & integer'image(to_integer(unsigned(StatRegCmps(test_no - 1)))) &
                       " Status Reg Errors: " & StatRegErr
                SEVERITY ERROR;
            end if;
	    if (StatRegChgErr /= "--------")  then
		REPORT "Failed Test #" & integer'image(test_no - 1) &
                       " - Status Register Change Failure -- " &
                       " Instruction: " & TestInstructions(test_no - 1) &
                       " IR: " & integer'image(to_integer(unsigned(IRTestVals(test_no - 1)))) &
                       " Status Register Change Errors: " & StatRegChgErr
                SEVERITY ERROR;
            end if;


	    -- save the previous status register value
	    PrevStatReg <= StatReg;


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
