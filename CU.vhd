 ----------------------------------------------------------------------------
--
--
-- Control Unit
--
-- RISC Control Unit for the AVR CPU. This contains the 16-bit instruction
-- register and logic for instruction decoding. The main inputs are from the
-- program data bus and status register, and it outputs the necessary control
-- signals for executing instructions, including data and program addressing,
-- ALU operations, status register controls, and register and IO operations.
-- For each decoded instruction, the appropriate control signals are outputted
-- for each cycle in the instruction.
--
-- Inputs:
--
-- ProgDB       - 16 bit program memory data bus
-- SReg         - 8 bit status flag bus
-- ZeroFlag     - 1 bit zero flag from ALU for CPSE op
-- SBFlag       - 1 bit skip bit flag from ALU for SBRC/SBRS op
-- CLK          - 1 bit system clock
-- RegIoFlag    - 1 bit flag, '1' if external address is in register range
-- RegIoSelFlag - 1 bit flag, external address is in either io or general range
-- DataAB       - 6 bit address to be remapped to registers
--
-- Outputs:
--
-- Immed        - 8 bit immediate value K
-- ImmedEn      - 1 bit mux ctrl signal for immed into ALU A Reg
-- RegWSel      - 5 bit register write select
-- RegSelA      - 5 bit register A select
-- RegSelB      - 5 bit register B select
-- IORegWEn     - 1 bit IO register write enable
-- IORegWSel    - 6 bit IO register write select
-- IndWEn       - 1 bit Indirect Addr write enable
-- IndAddrSel   - 1 bit Indirect Addr write select
-- IOOutSel     - 2 bit Mux ctrl signal for outputting IO reg to A Reg
-- DataRd       - 1 bit ctrl, indicates data memory is being read, active lo
-- DataWr       - 1 bit ctrl, indicates data memory is being written, active lo
-- IORegOutEn   - 1 bit IO command enable
-- ALUaddsub    - 3 bit ALU adder/subber operation signals
-- ALUsr        - 2 bit ALU shifter/rotator operation signals
-- ALUfop       - 4 bit ALU F Block operation signals
-- ALUcomneg    - 2 bit ALU com/neg operation signals
-- ALUSel       - 3 bit ALU output select
-- bitmask      - 8 bit mask for writing to status flags (SReg)
-- CPC          - 1 bit ctrl for signalling to ALU when CPC is the op
-- LoadIn       - 3 bit ctrl, selects data line into reg
-- SRegLd          - 1 bit select line to mux status reg source
-- DataOffsetSel   - 2 bit data address offset source select
-- PreSel          - 1 bit data pre/post address select
-- QOffset         - 6 bit address offset for data memory unit
-- DataDBWEn       - 1 bit DataDB write enable
-- DataABMux       - 1 bit DataAB mux control signal
--
-- Revision History:
-- 01/24/2019   Sophia Liu      Initial revision
-- 01/29/2019   Sundar Pandian  Testing Git, branches
-- 01/30/2019   Sundar Pandian  Initial architecture writeup
-- 01/31/2019   Sundar Pandian  added bitmask support
-- 02/01/2019   Sundar Pandian  debugged with testbench support
-- 02/06/2019   Sundar Pandian  Rewrote to match Glen's preferred structure
-- 02/07/2019   Sundar Pandian  Started adding support for AVR load/store instr
-- 02/09/2019   Sundar Pandian  Added documentation for AVR load/store ops
-- 02/09/2019   Sophia Liu      Updated and cleaned doc
-- 02/13/2019   Sundar Pandian  Fixed all ALU instructions
-- 02/25/2019   Sundar Pandian  combined into full CPU
-- 02/26/2019   Sundar Pandian  added EC support
-- 02/27/2019   Sophia Liu      Added header documentation
-- 02/27/2019   Sundar Pandian  Added documentation
--
----------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

use work.opcodes.all;
use work.constants.all;

entity CU is
    port(
        -- interrupt signals
        Reset   : in std_logic; -- hardware pin and watchdog reset
        INT0    : in std_logic; -- external interrupt request 0
        INT1    : in std_logic; -- external interrupt request 1
        T1CAP   : in std_logic; -- timer 1 capture event
        T1CPA   : in std_logic; -- timer 1 compare match A
        T1CPB   : in std_logic; -- timer 2 compare match B
        T1OVF   : in std_logic; -- timer 1 overflow
        T0OVF   : in std_logic; -- timer 0 overflow
        IRQSPI  : in std_logic; -- serial transfer complete
        UARTRX  : in std_logic; -- UART receive complete
        UARTRE  : in std_logic; -- UART data register empty
        UARTTX  : in std_logic; -- UART transmit complete
        ANACMP  : in std_logic; -- analog comparator

        ProgDB  : in std_logic_vector(ADDRSIZE-1 downto 0);     -- program memory data bus
        SReg    : in std_logic_vector(REGSIZE-1 downto 0);      -- status flags
        ZeroFlag: in std_logic;                                 -- zero flag from ALU for CPSE op
        SBFlag  : in std_logic;                                 -- skip bit flag from ALU for SBRC/SBRS op

        Immed       : out std_logic_vector(REGSIZE-1 downto 0); -- immediate value K
        ImmedEn     : out std_logic;                            -- mux ctrl signal for immed into ALU A Reg

        -- to register unit
        RegWEn      : out std_logic;                                -- register write enable
        RegWSel     : out std_logic_vector(RADDRSIZE-1 downto 0);   -- register write select
        RegSelA     : out std_logic_vector(RADDRSIZE-1 downto 0);   -- register A select
        RegSelB     : out std_logic_vector(RADDRSIZE-1 downto 0);   -- register B select
        IORegWEn    : out std_logic;                                -- IO register write enable
        IORegWSel   : out std_logic_vector(IOADDRSIZE-1 downto 0);  -- IO register write select
        IndWEn      : out std_logic;                                -- Indirect Addr write enable
        IndAddrSel  : out ADDR_SEL;                                 -- Indirect Addr write select
        IOOutSel    : out std_logic;                                -- Mux ctrl signal for outputting IO reg to A Reg

        DataRd          : out std_logic;                        -- indicates data memory is being read, active lo
        DataWr          : out std_logic;                        -- indicates data memory is being written, active lo

        -- to ALU and SReg
        ALUaddsub   : out ALU_ADDSUB;                           -- ALU adder/subber operation signals
        ALUsr       : out ALU_SR;                               -- ALU shifter/rotator operation signals
        ALUfop      : out ALU_FOPS;                             -- ALU F Block operation signals
        ALUcomneg   : out ALU_COMNEG;                           -- ALU com/neg operation signals
        ALUSel      : out ALU_SELECTS;                          -- ALU output select
        bitmask     : out BIT_MASK;                             -- mask for writing to flags (SReg)
        CPC         : out std_logic;                            -- bit for signalling to ALU when CPC is the op

        LoadIn      : out LOADIN_SEL;                           -- selects data line into reg

        -- Data memory access
        DataOffsetSel   : out OFFSET_SEL;                       -- data address offset source select
        PreSel          : out PREPOST_ADDR;                     -- data pre/post address select
        QOffset         : out std_logic_vector(Q_OFFSET_SIZE-1 downto 0); -- address offset for data memory unit
        DataDBWEn       : out std_logic;                        -- DataDB write enable
        DataABMux       : out std_logic;                        -- DataAB mux control signal

        -- CLK and address remapping signals
        CLK         : in std_logic;                           -- system clock
        RegIoFlag   : in std_logic;                           -- '1' if external address is in register range
        RegIoSelFlag: in std_logic;                           -- external address is in either io or general range
        DataAB      : in std_logic_vector(IOADDRSIZE-1 downto 0);  -- address to be remapped to registers

        -- Prog memory access
        Load            : out std_logic;                      -- PC load signal, active low
        ProgSourceSel   : out SOURCE_SEL;                     -- PC Source select line
        ProgIRSource    : out std_logic_vector(ADDRSIZE-1 downto 0) -- For outputting PC Source, (un)conditional addr
    );

end CU;

--
--  CU Architecture, contains IR register and decoder, Interrupt Event Handler, clock cycle FSM
--

architecture RISC of CU is
    signal cycle_num    :   OP_CYCLE;                                -- number of cycles of operation per instr
    signal cycle        :   std_logic_vector(1 downto 0);            -- current cycle of op for instr

    signal IR           :   std_logic_vector(ADDRSIZE-1 downto 0);   -- instruction register

    signal ProgDBLatch  :   std_logic_vector(ADDRSIZE-1 downto 0);   -- Prog Address Bus latch

    signal IntEventReg  :   std_logic_vector(INTREGSIZE-1 downto 0); -- interrupt event register
    signal Int_RJMPAddr :   std_logic_vector(INTREGSIZE-1 downto 0); -- interrupt JMP address
begin

    -- asynchronously decodes IR inputs
    decoder : process(IR, CLK, cycle, SBFlag, ZeroFlag, cycle_num, RegIoFlag, RegIoSelFlag, SReg, DataAB, ProgDB, ProgDBLatch, Int_RJMPAddr)
    begin
            -- sets cycle number for op_codes
            -- defaults operations to 1 cycle
            cycle_num   <= ONE_CYCLE;

            -- control signals default values, reset all write signal
            -- this way each operation only enables writes if necessary
            RegWEn      <= WRITE_DIS;
            IORegWEn    <= WRITE_DIS;
            IndWEn      <= WRITE_DIS;
            DataDBWEn   <= WRITE_DIS;
            -- disable immediate value muxing into ALU by default
            ImmedEn     <= IMM_DIS;
            -- default Immed register to IR(11..8) & IR(3..0)
            --  because general IR form follows: ----KKKK----KKKK
            Immed       <= IR(11 downto 8) & IR(3 downto 0);
            -- defaults to no bit changing in SReg
            bitmask     <= MASK_NONE;
            -- defaults to not CPC operation
            CPC         <= CPC_RST;
            -- set F Block to pass through B Register Value by default
            ALUfop      <= FOP_B;
            -- set COMNEG block to pass through A Register Value by default
            ALUcomneg   <= COMNEG_NONE;
            -- default writing outputs from Reg A back into Reg space
            LoadIn <= LD_REGA;
            -- default to outputting from register space, not IO
            IOOutSel <= REG_A_OUT;
            -- default to loading from indirect addressing, not direct memory
            DataABMux <= IND_ADDR;
            -- default DataRd and DataWr to inactive, active low signals
            DataRd <= '1';
            DataWr <= '1';
            -- default address offset value is 0
            DataOffsetSel <= ZERO_SEL;
            
            -- default PC to increment when operation is on last cycle
            --  else no change to PC
            if cycle = cycle_num - 1 then
                ProgSourceSel <= NORMAL_SRC;
            else
                ProgSourceSel <= RST_SRC;
            end if;
            
            -- load defaults to indirect addressing
            load <= PC_ADD;
            -- clear IO register write select line, allows for ALU default writing to SReg
            IORegWSel <= (others => '0');



            -- considering single byte adder/subber ops:
            -- ADC, ADD, SBC, SUB, CPC, CP
            -- all use same adder block in ALU with similar select line functionality
            if  std_match(IR, OpADC) or std_match(IR, OpADD) or
                std_match(IR, OpSBC) or std_match(IR, OpSUB) or
                std_match(IR, OpCPC) or std_match(IR, OpCP)  then
                --  000ooordddddrrrr

                LoadIn <= LD_ALU;                   -- load in data value from output of ALU

                RegSelA <= IR(8 downto 4);          -- Operand 1 is the first addend, loc in IR(8..4)

                RegWSel <= IR(8 downto 4);          -- Operand 1 is the register being written to, lock in IR(8..4)

                RegSelB <= IR(9) & IR(3 downto 0);  -- Operand 2 is the second addend, loc in IR(9)&IR(3..0)

                BitMask <= MASK_ADD;                -- SReg bits only changed by adder/subber block

                ALUSel <= ADDSUBOUT;                -- enable Adder/Subber output

                RegWEn <= IR(11);                   -- Writes data from RegA, mapped in IR(11)
                                                    -- only not set for CP, CPC. Don't rewrite reg value
                                                    -- IR(11) = '1' to rewrite Reg A
                                                    -- IR(11) = '0' to not rewrite Reg A

                ALUaddsub(SUBFLAG)  <= IR(11) xor IR(10);
                                                    -- subber flag mapped in IR as function of 2 bits
                                                    -- IR(11..10) = "00" or "11" to add
                                                    -- IR(11..10) = "01" or "10" to sub

                                                    -- carry/nborrow bit mapped in IR(12)

                if (IR(12) xor IR(11) xor IR(10)) = '1' then
                                                    -- if IR(12) = '1', carry in operation flips
                                                    --  for CP CPC vs ADC ADD SBC SUB                    
                    if (IR(11) xor IR(10)) = '1' then
                        ALUaddsub(CARRY_S1 downto CARRY_S0) <= NCARRY_IN;
                        if IR(10) = '1' then        -- special flag changing for CPC
                            CPC <= CPC_SET;         -- CPC set here in IR(10) with IR(11..10) = "01"
                        end if;
                    else
                        ALUaddsub(CARRY_S1 downto CARRY_S0) <= CARRY_IN;
                    end if;
                else
                                                    -- all no carry operations use same logic block with
                                                    -- carry in mapped in IR
                                                    --  clear active-hi carry for add
                                                    --  clear active-lo borrow for sub
                                                    -- maps same as to subFlag
                    if (IR(11) xor IR(10)) = '1' then
                                                    -- clearing borrow for subber, sets carry
                        ALUaddsub(CARRY_S1 downto CARRY_S0) <= SET_CARRY;
                    else
                                                    -- clearing carry for adder, resets carry
                        ALUaddsub(CARRY_S1 downto CARRY_S0) <= RST_CARRY;
                    end if;
                end if;
            end if;

            -- considering word adder/subber ops
            if  std_match(IR, OpADIW) or std_match(IR, OpSBIW) then
                -- 1001011oKKddKKKK

                LoadIn <= LD_ALU;                   -- load in data value from output of ALU

                RegWEn <= WRITE_EN;                 -- writes ALU output to register

                BitMask <= MASK_ADIW;               -- SReg bits only changed by adder/subber for word ops

                cycle_num <= TWO_CYCLES;            -- takes 2 cycles to complete operation

                ALUSel <= ADDSUBOUT;                -- enable Adder/Subber operation

                ALUaddsub(subFlag) <= IR(8);        -- subFlag mapped in IR directly

                ImmedEn <= IMM_EN;                  -- immediate value loads into second operand

                                                    -- value in IR is offset added to register 24
                                                    --  possible operands include {24, 26, 28, 30}
                                                    --  low byte operation uses above bytes while high byte
                                                    --    operation uses the next highest byte

                                                    -- if just loaded IR, doing first cycle
                if cycle = ZERO_CYCLES then
                                                    -- mapping to immediate value in IR, max value of 63
                                                    -- loc in IR(7..6)&IR(3..0)
                    Immed <= "00" & IR(7 downto 6) & IR(3 downto 0);
                                                    -- carry/nborrow cleared
                    if IR(8) = '1' then
                                                    -- carry/borrow set appropriately with subFlag
                        ALUaddsub(CARRY_S1 downto CARRY_S0) <= SET_CARRY;
                    else
                                                    -- borrow is set high when subbing
                        ALUaddsub(CARRY_S1 downto CARRY_S0) <= RST_CARRY;
                    end if;
                                                    -- limits operand addresses to those set previously
                                                    -- Operand 1 is the register being written to, lock in "11"&IR(5..4)&'0'
                    RegSelA <= "11" & IR(5 downto 4) & '0';
                                                    -- rewrite to op 1 register
                    RegWSel <= "11" & IR(5 downto 4) & '0';
                else
                    ALUfop <= FOP_ZERO;             -- add in zero in the second cycle
                                                    -- only bringing in carry from previous operation
                    if IR(8) = '1' then
                                                    -- flips carry to a borrow for subbing
                        ALUaddsub(CARRY_S1 downto CARRY_S0) <= NCARRY_IN;
                    else
                                                    -- carry out from low byte carries in to high byte add
                        ALUaddsub(CARRY_S1 downto CARRY_S0) <= CARRY_IN;
                    end if;
                                                    -- previous operand addresses + 1
                    RegSelA <= "11" & IR(5 downto 4) & '1';
                    RegWSel <= "11" & IR(5 downto 4) & '1';
                end if;
            end if;

            -- word multiply op
            if  std_match(IR, OpMUL) then
                -- 100111rdddddrrrr

                LoadIn <= LD_ALU;                   -- load in data value from output of ALU

                RegWEn <= WRITE_EN;                 -- writes ALU output to register

                cycle_num <= TWO_CYCLES;            -- takes 2 cycles to complete operation

                                                    -- enable MUL operation
                RegSelA <= IR(8 downto 4);          -- Operand 1 is the register, loc in IR(8..4)

                RegSelB <= IR(9) & IR(3 downto 0);  -- Operand 2 is the register, loc in IR(9)&IR(3..0)

                BitMask <= MASK_MUL;                -- SReg bits only changed by MUL block

                                                    -- output of MUL op is saved in R1:R0
                                                    --  low byte operation uses above bytes while high byte
                                                    --    operation uses the next highest byte
                                                    -- first do low byte multiply
                if cycle = ZERO_CYCLES then
                    ALUSel <= MULOUTL;              -- select low byte
                    RegWSel <= "00000";             -- writes to low byte first cycle
                else
                    ALUSel <= MULOUTH;              -- select high byte
                    RegWSel <= "00001";             -- writes to high byte second cycle
                end if;

                BitMask <= MASK_MUL;                -- SReg bits only changed by MUL block in ALU
            end if;

            -- considering immediate subber operations
            if  std_match(IR, OpSUBI) or std_match(IR, OpSBCI) or std_match(IR, OpCPI) then
                -- 0oooKKKKddddKKKK

                LoadIn <= LD_ALU;                   -- load in data value from output of ALU

                ALUSel <= ADDSUBOUT;                -- enable Adder/Subber operation

                if IR(12) = '0' then                -- carry/nborrow bit mapped in IR(12) directly
                    ALUaddsub(CARRY_S1 downto CARRY_S0) <= NCARRY_IN;
                else
                                                    -- all no carry operations use same logic block with
                                                    -- carry in mapped in IR
                                                    --  clear active-hi carry for add
                                                    --  clear active-lo borrow for sub
                                                    -- maps same as to subFlag
                    ALUaddsub(CARRY_S1 downto CARRY_S0) <= SET_CARRY;
                end if;
                                                    
                ALUaddsub(subFlag)  <= OP_SUB;      -- subbing so subFlag active
                                                    
                RegWEn <= IR(14);                   -- CPI doesn't rewrite register, mapped in IR(14) directly
                                                    
                ImmedEn <= IMM_EN;                  -- immediate value loads into second operand

                RegSelA <= '1' & IR(7 downto 4);    -- Operand 1 is the same register being written to, loc in '1'&IR(7..4)
                RegWSel <= '1' & IR(7 downto 4);    -- can only use registers in second half of register space

                BitMask <= MASK_ADD;                -- SReg bits only changed by subber block
            end if;

            -- considering incrementing/decrementing operations
            if  std_match(IR, OpINC) or std_match(IR, OpDEC) then
                -- 1001010dddddoo1o

                LoadIn <= LD_ALU;                   -- load in data value from output of ALU

                RegWEn <= WRITE_EN;                 -- writes ALU output to register

                ALUSel <= ADDSUBOUT;                -- enable Adder/Subber operation

                ALUaddsub(subFlag)  <= OP_ADD;      -- only using adder
                
                if IR(3) = '1' then                 -- if decrementing (mapped in IR(3))
                    ALUfop <= FOP_ONES;             -- add in -1 or "FF"
                                                    -- reset carry flag
                                                    -- equivalent to adding 2's complement of 1
                    ALUaddsub(CARRY_S1 downto CARRY_S0) <= RST_CARRY; 

                else                                -- if incrementing
                    ALUfop <= FOP_ZERO;             -- add in 0
                                                    -- set carry flag
                                                    -- equivalent to adding 1
                    ALUaddsub(CARRY_S1 downto CARRY_S0) <= SET_CARRY;
                end if;

                ImmedEn <= IMM_EN;                  -- immediate value loads into second operand

                RegSelA <= IR(8 downto 4);          -- Operand 1 is the first addend, loc in IR(8..4)
                RegWSel <= IR(8 downto 4);          -- Operand 1 is the register being written to

                BitMask <= MASK_DECINC;             -- SReg bits only changed by adder with +1 or -1 as Op B
            end if;

            -- considering COM and NEG operations
            if  std_match(IR, OpCOM) or std_match(IR, OpNEG) then
                -- 1001010ddddd000o

                LoadIn <= LD_ALU;                   -- load in data value from output of ALU

                ALUSel <= ADDSUBOUT;                -- enable Adder/Subber operation

                RegWEn <= WRITE_EN;                 -- writes ALU output to register

                                                    -- carry in mapped in IR
                                                    --  clear active-hi carry for add
                                                    --  clear active-lo borrow for sub
                                                    -- clear nborrow
                ALUaddsub(CARRY_S1 downto CARRY_S0) <= SET_CARRY;
                
                ALUaddsub(SUBFLAG)  <= OP_SUB;      -- always subbing so set

                                                    -- either subtract operand from xFF or x00
                                                    -- xFF for NEG
                                                    -- x00 for COM
                if IR(0) = '0' then                 -- COM operation mapped directly in IR(0)
                    ALUcomneg <= ALU_COM;
                else
                    ALUcomneg <= ALU_NEG;
                end if;

                RegSelB <= IR(8 downto 4);          -- Operand 2 is the register being written to, loc in IR(8..4)
                RegWSel <= IR(8 downto 4);          -- Argument is being subtracted from either xFF or x00

                if IR(0) = '0' then                 -- set bitmask based on if COM op or NEG op, mapped in IR(0)
                    BitMask <= MASK_COM;
                else
                    BitMask <= MASK_NEG;
                end if;
            end if;

            if  std_match(IR, OpAND) or std_match(IR, OpANDI) then
                -- 001000rdddddrrrr
                -- 0111KKKKddddKKKK

                LoadIn <= LD_ALU;                   -- load in data value from output of ALU

                ALUSel <= FBLOCKOUT;                -- enable F Block Operation

                RegWEn <= WRITE_EN;                 -- writes ALU output to register

                ALUfop <= FOP_AND;                  -- select AND operation

                if IR(14) = '1' then                -- if immediate operation
                    ImmedEn <= IMM_EN;              -- immediate value loads into second operand if ANDI op
                end if;

                RegWSel <= IR(8 downto 4);          -- Operand 1 is the same register being written to, loc in IR(8..4)
                RegSelA <= IR(8 downto 4);
                
                if IR(14) = '1' then                -- ANDI operation only maps to upper half of register space
                    RegSelA(4) <= '1';
                    RegWSel(4) <= '1';
                end if;

                RegSelB <= IR(9) & IR(3 downto 0);  -- Operand 2 is loc in IR(9) & IR(3..0)
                BitMask <= MASK_ANDOR;              -- SReg bits only changed by AND OR op in FBlock
            end if;

            if  std_match(IR, OpOR) or std_match(IR, OpORI) then
                -- 001010rdddddrrrr
                -- 0110KKKKddddKKKK

                LoadIn <= LD_ALU;                   -- load in data value from output of ALU

                ALUSel <= FBLOCKOUT;                -- enable F Block Operation

                RegWEn <= WRITE_EN;                 -- writes ALU output to register

                ALUfop <= FOP_OR;                   -- select OR operation

                if IR(14) = '1' then                -- if immediate operation
                    ImmedEn <= IMM_EN;              -- immediate value loads into second operand if ANDI op
                end if;

                RegWSel <= IR(8 downto 4);          -- Operand 1 is the same register being written to, loc in IR(8..4)
                RegSelA <= IR(8 downto 4);
                
                if IR(14) = '1' then                -- ORI operation only maps to upper half of register space
                    RegSelA(4) <= '1';
                    RegWSel(4) <= '1';
                end if;

                RegSelB <= IR(9) & IR(3 downto 0);  -- Operand 2 is loc in IR(9) & IR(3..0)
                BitMask <= MASK_ANDOR;              -- SReg bits only changed by AND OR op in FBlock
            end if;

            if  std_match(IR, OpEOR) then
                -- 001001rdddddrrrr

                LoadIn <= LD_ALU;                   -- load in data value from output of ALU

                ALUSel <= FBLOCKOUT;                -- enable F Block Operation

                RegWEn <= WRITE_EN;                 -- writes ALU output to register

                ALUfop <= FOP_XOR;                  -- select XOR operation

                RegSelA <= IR(8 downto 4);          -- Operand 1 is the same register being written to, loc in IR(8..4)
                RegWSel <= IR(8 downto 4);

                RegSelB <= IR(9) & IR(3 downto 0);  -- Operand 2 is loc in IR(9) & IR(3..0)
                
                BitMask <= MASK_EOR;                -- SReg bits only changed by EOR op in FBlock
            end if;

            if  std_match(IR, OpLSR) then
                -- 1001010ddddd0110

                LoadIn <= LD_ALU;                   -- load in data value from output of ALU

                ALUSel <= SHIFTOUT;                 -- enable F Block Operation

                RegWEn <= WRITE_EN;                 -- writes ALU output to register

                ALUsr <= SR_LSR;                    -- select LSR operation

                RegSelA <= IR(8 downto 4);          -- Operand 1 is the same register being written to, loc in IR(8..4)
                RegWSel <= IR(8 downto 4);

                BitMask <= MASK_SHIFT;              -- SReg bits only changed by shift op
            end if;

            if  std_match(IR, OpASR) then
                -- 1001010ddddd0101

                LoadIn <= LD_ALU;                   -- load in data value from output of ALU

                ALUSel <= SHIFTOUT;                 -- enable F Block Operation

                RegWEn <= WRITE_EN;                 -- writes ALU output to register
                
                ALUsr <= SR_ASR;                    -- select ASR operation

                RegSelA <= IR(8 downto 4);          -- Operand 1 is the same register being written to, loc in IR(8..4)
                RegWSel <= IR(8 downto 4);
                
                BitMask <= MASK_SHIFT;              -- SReg bits only changed by shift op
            end if;

            if  std_match(IR, OpROR) then
                -- 1001010ddddd0111

                LoadIn <= LD_ALU;                   -- load in data value from output of ALU

                ALUSel <= SHIFTOUT;                 -- enable F Block Operation

                RegWEn <= WRITE_EN;                 -- writes ALU output to register

                ALUsr <= SR_ROR;                    -- select ROR operation

                                                    -- uses carry in from last operation
                ALUaddsub(CARRY_S1 downto CARRY_S0) <= CARRY_IN;

                RegSelA <= IR(8 downto 4);          -- Operand 1 is the same register being written to, loc in IR(8..4)
                RegWSel <= IR(8 downto 4);

                BitMask <= MASK_SHIFT;              -- SReg bits only changed by shift op
            end if;

            if  std_match(IR, OpBCLR) or std_match(IR, OpBSET) then
                -- 10010100osss1000

                if IR(7) = '0' then                 -- BSET operation directly mapped in IR(7)
                    ALUSel <= BSET;                 -- set ALU SReg output for BSET
                else
                    ALUSel <= BCLR;                 -- set ALU SReg output for BCLR
                end if;

                bitmask <= (others => '0');         -- clear bitmask
                                                    --  then set proper bit high in bitmask
                                                    --  loc in IR(6..4)
                                                    -- bitmask uses active hi to enable bit changing
                bitmask(conv_integer(IR(6 downto 4))) <= '1';
            end if;

            if  std_match(IR, OpBLD) or std_match(IR, OpBST) then
                -- 111110odddddobbb

                LoadIn <= LD_ALU;                   -- load in data value from output of ALU

                ImmedEn <= IMM_EN;                  -- immediate value loads into second operand

                ALUSel <= BOUT;                     -- sets BOUT block output from ALU

                RegSelA <= IR(8 downto 4);          -- Operand is same as register being written to, loc in IR(8..4)
                RegWSel <= IR(8 downto 4);

                BitMask <= (others => '0');         -- clear bitmask

                if IR(T_IR) = '0' then              -- if writing back to register, mapped in IR(T_IR)
                    RegWEn <= WRITE_EN;             -- writes back if doing BLD, loading into register from T bit
                end if;

                BitMask(T_SREG) <= IR(T_IR);        -- SReg bits only changes T bit if mapped in IR(T_IR) for BST op
            end if;

            if std_match(IR, OpSWAP) then
                -- 1001010ddddd0010

                LoadIn <= LD_ALU;                   -- load in data value from output of ALU

                RegWEn <= WRITE_EN;                 -- writes ALU output to register

                ALUSel <= SWAPOUT;                  -- select ALU to output from SWAP block

                RegSelA <= IR(8 downto 4);          -- Operand is same as register being written to, loc in IR(8..4)
                RegWSel <= IR(8 downto 4);

                BitMask <= MASK_NONE;               -- SReg bits not changed
            end if;

            if std_match(IR, OpIN) or std_match(IR, OpOUT) then
                -- 1011oppdddddpppp

                RegSelA     <= IR(8 downto 4);      -- Operand is same as register being written to, loc in IR(8..4)
                RegWSel     <= IR(8 downto 4);

                                                    -- io reg operand loc in IR(10..9) & IR(3..0)
                IORegWSel   <= IR(10 downto 9) & IR(3 downto 0);

                RegWEn      <= not IR(11);          -- writing to reg space if doing IN op, mapped in opposite of IR(11)

                IOOutSel    <= not IR(11);          -- output from Reg Unit if doing IN op, mapped in opposite of IR(11)

                IORegWEn    <= IR(11);              -- writing to io reg space if doing OUT op, mapped in IR(11)
            end if;

            if  std_match(IR, OpLDX) or
                std_match(IR, OpLDXI) or
                std_match(IR, OpLDXD) or
                std_match(IR, OpLDYI) or
                std_match(IR, OpLDYD) or
                std_match(IR, OpLDZI) or
                std_match(IR, OpLDZD) then
                -- 1001000dddddoooo

                    cycle_num <= TWO_CYCLES;            -- takes 2 cycles to complete operation

                    LoadIn <= LD_DB;                    -- loading values into register space from DataDB

                    DataOffsetSel <= IR(1 downto 0);    -- offset values for 0, +1, -1 stored in low two bits of IR, IR(1..0)
                                                        -- add  0 -> "00" = ZERO_SEL
                                                        -- add +1 -> "01" = INC_SEL
                                                        -- add -1 -> "10" = DEC_SEL

                    PreSel <= IR(1);                    -- pre flag setting stored in IR(1)
                                                        -- pre-op -> IR(1) = '1' = PRE_ADDR
                                                        -- pre-op -> IR(0) = '0' = POST_ADDR

                    IndAddrSel <= IR(3 downto 2);       -- indirect addressing stored in IR(3..2)
                                                        -- X -> IR(3..2) = "11" = X_SEL
                                                        -- Y -> IR(3..2) = "10" = Y_SEL
                                                        -- Z -> IR(3..2) = "00" = Z_SEL

                    RegWSel <= IR(8 downto 4);          -- Operand 1 is the register being written to, loc in IR(8..4)

                    if cycle = ZERO_CYCLES then         -- during first cycle
                    else                                -- during second cycle
                        IndWEn <= WRITE_EN;             -- write result of arith block back to indirect address reg

                        DataRd <= CLK;                  -- DataRd = CLK for the second cycle, so will go active low at end

                        RegWEn <= WRITE_EN;             -- Write data from DataDB into register space
                    end if;
            end if;

            if  std_match(IR, OpLDDY) or std_match(IR, OpLDDZ) then
                -- 10q0qq0dddddoqqq

                    cycle_num <= TWO_CYCLES;            -- takes 2 cycles to complete operation

                    LoadIn <= LD_DB;                    -- loading values into register space from DataDB

                                                        -- offset values is the q offset, encoded in the IR
                                                        -- all q bits as seen above: IR(13)&IR(11..10)&IR(2..0)
                    QOffset <= IR(13) & IR(11 downto 10) & IR(2 downto 0);

                    DataOffsetSel <= OFFS_SEL;          -- Data Offset is the q offset value

                    PreSel <= PRE_ADDR;                 -- pre flag set to output to address bus whenever add op finished

                    IndAddrSel <= IR(3) & '0';          -- indirect addressing stored in IR(3), while setting LSB = '0'
                                                        -- Y -> IR(3) & '0' = "10" = Y_SEL
                                                        -- Z -> IR(3) & '0' = "00" = Z_SEL

                    RegWSel <= IR(8 downto 4);          -- Operand 1 is the register being written to, loc in IR(8..4)

                    if cycle = ZERO_CYCLES then         -- during first cycle
                        -- do nothing
                    else                                -- during second cycle
                        DataRd <= CLK;                  -- DataRd = CLK for the second cycle, so will go active low at end

                        RegWEn <= WRITE_EN;             -- Write data from DataDB into register space
                    end if;
            end if;

            if  std_match(IR, OpLDS) then
                -- 1001000ddddd0000

                    cycle_num <= THREE_CYCLES;          -- takes 3 cycles to complete operation

                    LoadIn <= LD_DB;                    -- loading values into register space from DataDB


                    RegWSel <= IR(8 downto 4);          -- Operand 1 is the register being written to, loc in IR(8..4)

                    if cycle = ZERO_CYCLES then         -- during first cycle
                        ProgSourceSel <= NORMAL_SRC;    -- increment PC here so ProgAB points to next IR
                        -- do nothing
                    elsif cycle = ONE_CYCLE then        -- during second cycle
                        DataABMux <= MEM_ADDR;          -- signal to latch and output ProgDB memory on DataAB
                    else                                -- during third cycle
                        DataABMux <= MEM_ADDR;          -- still outputting ProgDB to DataAB

                        DataRd <= CLK;                  -- DataRd = CLK for the third cycle, so will go active low at end

                        RegWEn <= WRITE_EN;             -- write data from DataDB into register
                    end if;
            end if;

            if  std_match(IR, OpSTX) or
                std_match(IR, OpSTXI) or
                std_match(IR, OpSTXD) or
                std_match(IR, OpSTYI) or
                std_match(IR, OpSTYD) or
                std_match(IR, OpSTZI) or
                std_match(IR, OpSTZD) then
                -- 1001001dddddoooo

                    cycle_num <= TWO_CYCLES;            -- takes 2 cycles to complete operation

                    -- loading values from RegA into DataDB so no change from default

                    DataOffsetSel <= IR(1 downto 0);    -- offset values for 0, +1, -1 stored in low two bits of IR, IR(1..0)
                                                        -- add  0 -> "00" = ZERO_SEL
                                                        -- add +1 -> "01" = INC_SEL
                                                        -- add -1 -> "10" = DEC_SEL

                    PreSel <= IR(1);                    -- pre flag setting stored in IR(1)
                                                        -- pre-op -> IR(1) = '1' = PRE_ADDR
                                                        -- pre-op -> IR(0) = '0' = POST_ADDR

                    IndAddrSel <= IR(3 downto 2);       -- indirect addressing stored in IR(3..2)
                                                        -- X -> IR(3..2) = "11" = X_SEL
                                                        -- Y -> IR(3..2) = "10" = Y_SEL
                                                        -- Z -> IR(3..2) = "00" = Z_SEL

                    RegSelA <= IR(8 downto 4);          -- Operand 1 is the register being read from, loc in IR(8..4)

                    if cycle = ZERO_CYCLES then         -- during first cycle
                    else                                -- during second cycle
                        IndWEn <= WRITE_EN;             -- write result of arith block back to indirect address reg

                        DataWr <= CLK;                  -- DataWr = CLK for the second cycle, so will go active low at end

                        DataDBWEn <= WRITE_EN;          -- Write data from register into DataDB
                    end if;
            end if;

            if  std_match(IR, OpSTDZ) or std_match(IR, OpSTDY) then
                -- 10q0qq1rrrrroqqq

                    cycle_num <= TWO_CYCLES;            -- takes 2 cycles to complete operation

                    -- loading values from RegA into DataDB so no change from default

                                                        -- offset values is the q offset, encoded in the IR
                                                        -- all q bits as seen above: IR(13)&IR(11..10)&IR(2..0)
                    QOffset <= IR(13) & IR(11 downto 10) & IR(2 downto 0);

                    DataOffsetSel <= OFFS_SEL;          -- Data Offset is the q offset value

                    PreSel <= PRE_ADDR;                 -- pre flag set to output to address bus whenever add op finished

                    IndAddrSel <= IR(3) & '0';          -- indirect addressing stored in IR(3), while setting LSB = '0'
                                                        -- Y -> IR(3) & '0' = "10" = Y_SEL
                                                        -- Z -> IR(3) & '0' = "00" = Z_SEL

                    RegSelA <= IR(8 downto 4);          -- Operand 1 is the register being read from, loc in IR(8..4)

                    if cycle = ZERO_CYCLES then         -- during first cycle
                        -- do nothing
                    else                                -- during second cycle
                        DataWr <= CLK;                  -- DataRd = CLK for the third cycle, so will go active low at end

                        DataDBWEn <= WRITE_EN;          -- write data from register into DataDB
                    end if;
            end if;

            if  std_match(IR, OpSTS) then
                -- 1001001rrrrr0000

                    cycle_num <= THREE_CYCLES;          -- takes 3 cycles to complete operation

                    -- loading values into DataDB from RegA so no change from default

                    RegSelA <= IR(8 downto 4);          -- Operand 1 is the register being written to, loc in IR(8..4)

                    if cycle = ZERO_CYCLES then         -- during first cycle
                        ProgSourceSel <= NORMAL_SRC;    -- increment PC here so ProgAB points to next IR
                        -- do nothing
                    elsif cycle = ONE_CYCLE then        -- during second cycle
                        DataABMux <= MEM_ADDR;          -- signal to latch and output ProgDB memory on DataAB
                    else                                -- during third cycle
                        DataABMux <= MEM_ADDR;          -- still outputting ProgDB to DataAB

                        DataWr <= CLK;                  -- DataWr = CLK for the third cycle, so will go active low at end

                        DataDBWEn <= WRITE_EN;          -- write data from mem space addr into DataDB
                    end if;
            end if;

            if  std_match(IR, OpLDI) then
                -- 1110kkkkddddkkkk
                    -- takes 1 cycle to complete operation so no change from default

                    LoadIn <= LD_IMM;                   -- loading values into register space from Immed
                    --ImmedEn <= IMM_EN;
                    --ALUSel <= FBLOCKOUT;
                    --ALUfop <= FOP_B;

                    RegWSel <= '1' & IR(7 downto 4);    -- Operand 1 is the register being written to
                                                        -- Immediate operations limited to upper half of register space
                                                        -- so, MSB of RADDRSIZE = '1' and rest is loc in IR(7..4)

                                                        -- immediate value found in k locs in IR, IR(11..8)&IR(3..0)
                    Immed <= IR(11 downto 8) & IR(3 downto 0);

                    RegWEn <= WRITE_EN;                 -- write data from Immed into register space
            end if;

            if  std_match(IR, OpPOP) then
                -- 1001000ddddd1111

                    cycle_num <= TWO_CYCLES;            -- takes 2 cycles to complete operation

                    LoadIn <= LD_DB;                    -- loading values into register space from DataDB

                    DataOffsetSel <= INC_SEL;           -- Popping pre-increments
                    PreSel <= PRE_ADDR;                 --  the Stack Pointer

                    IndAddrSel <= SP_SEL;               -- indirect addressing stored in SP

                    RegWSel <= IR(8 downto 4);          -- Operand 1 is the register being written to, loc in IR(8..4)

                    if cycle = ZERO_CYCLES then         -- during first cycle
                    else                                -- during second cycle
                        IndWEn <= WRITE_EN;             -- write result of arith block back to indirect address reg

                        DataRd <= CLK;                  -- DataRd = CLK for the second cycle, so will go active low at end

                        RegWEn <= WRITE_EN;             -- Write data from DataDB into register space
                    end if;
            end if;

            if  std_match(IR, OpPUSH) then
                -- 1001001rrrrr1111

                    cycle_num <= TWO_CYCLES;            -- takes 2 cycles to complete operation

                    -- loading values from RegA into DataDB so no change from default

                    DataOffsetSel <= DEC_SEL;           -- Pushing post decrements
                    PreSel <= POST_ADDR;                --  the Stack Pointer

                    IndAddrSel <= SP_SEL;               -- indirect addressing stored in Stack Pointer

                    RegSelA <= IR(8 downto 4);          -- Operand 1 is the register being read from, loc in IR(8..4)
                    -- during first cycle
                    if cycle = ZERO_CYCLES then
                    else                                -- during second cycle
                        IndWEn <= WRITE_EN;             -- write result of arith block back to indirect address reg

                        DataWr <= CLK;                  -- DataWr = CLK for the second cycle, so will go active low at end

                        DataDBWEn <= WRITE_EN;          -- Write data from register into DataDB
                    end if;
            end if;

            if  std_match(IR, OpMOV) then
                --001011rdddddrrrr
                    -- takes 1 cycle to complete operation so no change from default

                    -- loading values from RegA into RegB so no change from default loading

                    RegWSel <= IR(8 downto 4);          -- Operand 1 is the register being written to, loc in IR(8..4)

                    RegSelA <= IR(9) & IR(3 downto 0);  -- Operand 2 is the register being read from, loc in IR(9)&IR(3..0)

                    RegWEn <= WRITE_EN;                 -- writing to register
            end if;

            -- handle changing address source
            if  RegIoFlag = '1' then
                if (std_match(IR, OpSTS) or
                std_match(IR, OpSTDZ) or
                std_match(IR, OpSTDY) or
                std_match(IR, OpSTX) or
                std_match(IR, OpSTXI) or
                std_match(IR, OpSTXD) or
                std_match(IR, OpSTYI) or
                std_match(IR, OpSTYD) or
                std_match(IR, OpSTZI) or
                std_match(IR, OpSTZD)) then
                -- writing to reg/io at data address
                if cycle /= ZERO_CYCLES then
                    DataDBWEn <= WRITE_DIS; -- disable data db
                    DataWr <= '1';          -- not writing to data db
                    if RegIoSelFlag = REG_A_OUT then    -- if writing to register
                        RegWEn <= WRITE_EN;             -- enable reg writing
                        RegWSel <= dataAB(4 downto 0);  -- set reg address
                    else                                -- if writing to io
                        IORegWEn <= WRITE_EN;           -- enable io writing
                        IORegWSel <= dataAB(5 downto 0); -- set io address
                    end if;
                end if;

               elsif std_match(IR, OpLDS) or
                std_match(IR, OpLDDY) or
                std_match(IR, OpLDDZ) or
                std_match(IR, OpLDX) or
                std_match(IR, OpLDXI) or
                std_match(IR, OpLDXD) or
                std_match(IR, OpLDYI) or
                std_match(IR, OpLDYD) or
                std_match(IR, OpLDZI) or
                std_match(IR, OpLDZD) then
               -- reading from reg at data address
               LoadIn <= LD_REGA;           -- set load source to registers
               if cycle /= ZERO_CYCLES then
                    DataRd <= '1';          -- not reading from db
                    if RegIoSelFlag = REG_A_OUT then    -- if reading from register
                        RegSelA <= dataAB(4 downto 0);  -- set reg address
                    else                                -- otherwise reading from io
                        IORegWSel <= dataAB(5 downto 0); -- set io address
                        IOOutSel <= IO_OUTPUT;           -- set reg unit output to io regs
                    end if;
                end if;
                end if;
            end if;

            if std_match(IR, OpJMP) then
                -- 1001010aaaaa110a
                -- aaaaaaaaaaaaaaaa

                cycle_num <= THREE_CYCLES;          -- takes 3 cycles to complete operation

                if cycle = ZERO_CYCLES then         -- during first cycle
                    ProgSourceSel <= NORMAL_SRC;    -- increment PC here so ProgAB points to next IR
                    ProgDBLatch <= ProgDB;          -- latch ProgDB, so can change ProgAB to JMP location

                elsif cycle = ONE_CYCLE then        -- during second cycle
                    ProgIRSource <= ProgDBLatch;    -- output the latched ProgDB so can load new one
                    ProgSourceSel <= IR_SRC;        -- CU decides ProgAB source
                    load <= PC_LOAD;                -- direct addressing mode so loading new source

                else                                -- during third cycle
                    ProgSourceSel <= RST_SRC;       -- no change to ProgAB since pointing to next IR already
                end if;
            end if;

            if std_match(IR, OpRJMP) then
                -- 1100jjjjjjjjjjjj

                cycle_num <= TWO_CYCLES;            -- takes 2 cycles to complete operation

                if cycle = ZERO_CYCLES then         -- during first cycle
                    ProgSourceSel <= NORMAL_SRC;    -- increment PC here so ProgAB points to next IR

                else                                -- during second cycle
                                                    -- indirect address jmp loc in IR(11..0)
                    ProgIRSource(11 downto 0) <= IR(11 downto 0);
                                                    -- sign extend top bit
                    ProgIRSource(15 downto 12) <= (others => IR(11));
                                                    -- CU decides ProgAB source
                    ProgSourceSel <= IR_SRC;
                end if;
            end if;

            if std_match(IR, OpIJMP) then
                -- 10010100XXXX1001

                cycle_num <= TWO_CYCLES;            -- takes 2 cycles to complete operation

                DataOffsetSel <= ZERO_SEL;          -- output Z value with no inc or dec

                IndAddrSel <= Z_SEL;                -- get Z value for JMP

                if cycle = ZERO_CYCLES then         -- during first cycle
                    ProgSourceSel <= NORMAL_SRC;    -- increment PC here so ProgAB points to next IR

                else                                -- during second cycle
                    ProgSourceSel <= Z_SRC;         -- Z register is ProgAB source
                    load <= PC_LOAD;                -- direct addressing so loading new source
                end if;
            end if;

            if std_match(IR, OpCALL) then
                -- 1001010aaaaa111a
                -- aaaaaaaaaaaaaaaa

                cycle_num <= ZERO_CYCLES;           -- takes 4 cycles to complete operation

                DataOffsetSel <= DEC_SEL;           -- Pushing post decrements
                PreSel <= POST_ADDR;                --  the Stack Pointer

                IndAddrSel <= SP_SEL;               -- indirect addressing stored in Stack Pointer

                if cycle = ZERO_CYCLES then         -- during first cycle
                    ProgDBLatch <= ProgDB;          -- latch ProgAB value, Prog addr to call
                    ProgSourceSel <= NORMAL_SRC;    -- inc PC to point to next value, our address of subroutine

                elsif cycle = ONE_CYCLE then        -- during second cycle
                    ProgSourceSel <= NORMAL_SRC;    -- inc PC to point to next value, now pointing to next op IR

                elsif cycle = TWO_CYCLES then       -- during third cycle
                    IndWEn <= WRITE_EN;             -- write result of arith block back to indirect address reg

                    LoadIn <= LD_PROG_HI;           -- load high byte of next IR into DataDB to
                                                    --  save into stack

                    DataWr <= CLK;                  -- DataWr = CLK for the second cycle, so will go active low at end

                    DataDBWEn <= WRITE_EN;          -- Write data from register into DataDB

                    ProgSourceSel <= RST_SRC;       -- hold PC value here, pointing to next op IR

                else                                -- during fourth cycle
                    IndWEn <= WRITE_EN;             -- write result of arith block back to indirect address reg
                    LoadIn <= LD_PROG_LO;           -- load high byte of next IR into DataDB to
                                                    --  save into stack

                    DataWr <= CLK;                  -- DataWr = CLK for the second cycle, so will go active low at end

                    DataDBWEn <= WRITE_EN;          -- Write data from register into DataDB

                    ProgIRSource <= ProgDBLatch;    -- output latched ProgAB value

                    ProgSourceSel <= IR_SRC;        -- output address of subroutine

                    load <= PC_LOAD;                -- direct addressing so loading address, not adding
                end if;
            end if;

            if std_match(IR, OpCALLI) then
                -- 0000000000000001

                cycle_num <= THREE_CYCLES;          -- takes 4 cycles to complete operation

                DataOffsetSel <= DEC_SEL;           -- Pushing post decrements
                PreSel <= POST_ADDR;                --  the Stack Pointer

                IndAddrSel <= SP_SEL;               -- indirect addressing stored in Stack Pointer

                if cycle = ZERO_CYCLES then         -- during first cycle
                    BitMask <= MASK_INT;            -- SReg flips I bit, now should be disabled unless mismatched interrupt calls

                elsif cycle = ONE_CYCLE then        -- during second cycle
                    LoadIn <= LD_PROG_HI;           -- load high byte of next IR into DataDB to
                                                    --  save into stack

                    DataWr <= CLK;                  -- DataWr = CLK for the second cycle, so will go active low at end

                    DataDBWEn <= WRITE_EN;          -- Write data from register into DataDB

                    ProgSourceSel <= RST_SRC;       -- hold PC value here, pointing to next op IR

                    IndWEn <= WRITE_EN;             -- write result of arith block back to indirect address reg

                else                                -- during third cycle
                    IndWEn <= WRITE_EN;             -- write result of arith block back to indirect address reg
                    LoadIn <= LD_PROG_LO;           -- load high byte of next IR into DataDB to
                                                    --  save into stack

                    DataWr <= CLK;                  -- DataWr = CLK for the second cycle, so will go active low at end

                    DataDBWEn <= WRITE_EN;          -- Write data from register into DataDB

                                                    -- ZERO PAD RJMP address since Interrupt Vector table starts at 0000 in 
                                                    --  memory. all relative addressing adds pointing to later instructions
                                                    -- RJMP address was determined by highest priority interrupt enable
                    ProgIRSource  <= INT_ZERO_PAD & Int_RJMPAddr;

                    ProgSourceSel <= IR_SRC;        -- CU decides ProgAB source 

                    load <= PC_LOAD;                -- check this TODO
                end if;
            end if;

            if std_match(IR, OpRCALL) or std_match(IR, OpICALL) then
            -- 1101jjjjjjjjjjjj - RCALL Opcode
            -- 10010101XXXX1001 - ICALL Opcode

                cycle_num <= THREE_CYCLES;          -- takes 4 cycles to complete operation

                DataOffsetSel <= DEC_SEL;           -- Pushing post decrements
                PreSel <= POST_ADDR;                --  the Stack Pointer

                IndAddrSel <= SP_SEL;               -- indirect addressing stored in Stack Pointer

                if cycle = ZERO_CYCLES then         -- during first cycle
                    ProgSourceSel <= NORMAL_SRC;    -- inc PC value here, pointing to next op IR

                elsif cycle = ONE_CYCLE then        -- during second cycle
                    LoadIn <= LD_PROG_HI;           -- load high byte of next IR into DataDB to
                                                    --  save into stack

                    DataWr <= CLK;                  -- DataWr = CLK for the second cycle, so will go active low at end

                    DataDBWEn <= WRITE_EN;          -- Write data from register into DataDB

                    ProgSourceSel <= RST_SRC;       -- hold PC value here, pointing to next op IR

                    IndWEn <= WRITE_EN;             -- write result of arith block back to indirect address reg

                else                                -- during third cycle
                    IndWEn <= WRITE_EN;             -- write result of arith block back to indirect address reg
                    
                    LoadIn <= LD_PROG_LO;           -- load high byte of next IR into DataDB to
                                                    --  save into stack

                    DataWr <= CLK;                  -- DataWr = CLK for the second cycle, so will go active low at end

                    DataDBWEn <= WRITE_EN;          -- Write data from register into DataDB

                    if IR(14) = '1' then            -- then RCALL op
                                                    -- relative calling so adding to prev PC value
                                                    -- loc in IR(11..0)
                        ProgIRSource(11 downto 0) <= IR(11 downto 0);
                                                    -- sign extend top bit for negative relative addressing
                        ProgIRSource(15 downto 12) <= (others => IR(11));

                        ProgSourceSel <= IR_SRC;    -- CU decides ProgAB source

                    else                            -- then ICALL op
                        ProgSourceSel <= Z_SRC;     -- uses Z value
                        load <= PC_LOAD;            -- loading not adding
                    end if;
                end if;
            end if;

            if std_match(IR, OpRET) or std_match(IR, OpRETI) then
                -- 100101010xxo1000

                cycle_num <= ZERO_CYCLES;           -- takes 4 cycles to complete operation

                DataOffsetSel <= INC_SEL;           -- Pushing post decrements
                PreSel <= PRE_ADDR;                 --  the Stack Pointer

                IndAddrSel <= SP_SEL;               -- indirect addressing stored in Stack Pointer

                if cycle = ZERO_CYCLES then         -- during first cycle

                elsif cycle = ONE_CYCLE then        -- during second cycle
                    IndWEn <= WRITE_EN;             -- write result of arith block back to indirect address reg
                                                    --  incremented SP val is written back into SP reg
                    
                    DataRd <= CLK;                  -- DataRd = CLK for the second cycle, so will go active low at end

                    ProgSourceSel <= DB_LO_SRC;     -- low byte of ProgAB return address being popped from stack
                    load <= PC_LOAD;                -- load in this low byte value

                elsif cycle = TWO_CYCLES then       -- during third cycle

                else                                -- during fourth cycle
                    IndWEn <= WRITE_EN;             -- write result of arith block back to indirect address reg
                                                    --  incremented SP val is written back into SP reg
                    
                    DataRd <= CLK;                  -- DataRd = CLK for the second cycle, so will go active low at end

                    ProgSourceSel <= DB_HI_SRC;     -- high byte of ProgAB return address being popped from stack
                                                    -- gets added to prev ProgAB value containing low byte
 
                    if IR(4) = '1' then
                        BitMask <= MASK_INT;        -- SReg flips I bit, now should be disabled unless mismatched interrupt calls
                    end if;
                end if;
            end if;

            if std_match(IR, OpBRBC) or std_match(IR, OpBRBS) then
                -- 11110orrrrrrrbbb

                -- IR(2..0) contains bit value to check
                -- not IR(10) contains branch condition
                if SReg(to_integer(unsigned(IR(2 downto 0)))) = not IR(10) then
                    cycle_num <= TWO_CYCLES;        -- if branching, two cycles of op
                else
                    cycle_num <= ONE_CYCLE;         -- if not branching, one cycle of op
                end if;

                if cycle = ZERO_CYCLES then
                    ProgSourceSel <= NORMAL_SRC;    -- if first cycle of op, point to next ProgAB value
                else
                                                    -- check value and if in second cycle of op, must be branching
                    ProgIRSource(6 downto 0) <= IR(9 downto 3);
                                                    -- IR branch loc located in IR(9..3)
                    ProgIRSource(15 downto 7) <= (others => IR(9));
                                                    -- since relative addressing, sign extend top bit
                    ProgSourceSel <= IR_SRC;        -- CU decides IR source
                end if;
            end if;

            if std_match(IR, OpCPSE) then
                --000100rdddddrrrr

                BitMask <= MASK_NONE;               -- no SReg bits changed

                RegSelA <= IR(8 downto 4);          -- Operand 1 is the first addend, loc in IR(8..4)

                RegSelB <= IR(9) & IR(3 downto 0);  -- Operand 2 is the second addend, loc in IR(9)&IR(3..0)

                ALUSel <= ADDSUBOUT;                -- enable Adder/Subber output

                ALUaddsub(SUBFLAG)  <= OP_SUB;      -- subtracting Op 2 from Op 1 to check if equal
                                                    -- no carry in so set borrow
                ALUaddsub(CARRY_S1 downto CARRY_S0) <= SET_CARRY;

                ProgSourceSel <= NORMAL_SRC;        -- always increments ProgAB since skip instructions
                                                    --  just let IR change to next instr or hold it

                -- can only change cycle_num on first cycle of op
                if cycle = ZERO_CYCLES and ZeroFlag = '1' then
                                                    -- if skipping
                    if std_match(ProgDB, OpLDS) or std_match(ProgDB, OpSTS) or
                       std_match(ProgDB, OpJMP) or std_match(ProgDB, OpCALL) then
                        cycle_num <= THREE_CYCLES;  -- only these instr have a second word
                    else
                        cycle_num <= TWO_CYCLES;    -- else just skip one ProgAB value
                    end if;
                else
                    cycle_num <= cycle_num;         -- hold cycle_num value set during first cycle of op
                end if;

            end if;

            if std_match(IR, OpSBRC) or std_match(IR, OpSBRS) then
                --111111orrrrrXbbb

                ImmedEn <= IMM_EN;                  -- muxing in Immed value into Operand 2

                RegSelA <= IR(8 downto 4);          -- Operand 1 is the value being checked, loc in IR(8..4)

                ProgSourceSel <= NORMAL_SRC;        -- always increments ProgAB since skip instructions
                                                    --  just let IR change to next instr or hold it

                -- can only change cycle_num on first cycle of op
                if cycle = ZERO_CYCLES and ZeroFlag = '1' then
                                                    -- if skipping
                    if std_match(ProgDB, OpLDS) or std_match(ProgDB, OpSTS) or
                       std_match(ProgDB, OpJMP) or std_match(ProgDB, OpCALL) then
                        cycle_num <= THREE_CYCLES;  -- only these instr have a second word
                    else
                        cycle_num <= TWO_CYCLES;    -- else just skip one ProgAB value
                    end if;
                else
                    cycle_num <= cycle_num;         -- hold cycle_num value set during first cycle of op
                end if;

            end if;

            if std_match(IR, OpNOP) then
                -- 0000000000000000
                -- do nothing
            end if;

            if std_match(IR, OpSLP) then
                -- 10010101100X1000
                -- do nothing
            end if;

            if std_match(IR, OpWDR) then
                -- 10010101101X1000
                -- do nothing
            end if;

    end process decoder;

    -- Fetches next instruction when on last cycle of operation of previous instruction
    -- cycle value is zero indexed so final value is one less than cycle_num
    IR_update: process (CLK, IntEventReg, cycle, cycle_num, ProgDB)
    begin
        if (rising_edge(CLK)) then
            if Reset = '0' then                     -- if reset, load IR with NOP
                IR <= OpNOP;
            elsif cycle = cycle_num-1 then          -- if on last cycle of op
                if IntEventReg = NO_INT or IntEventReg = RESET_INT then
                    IR <= ProgDB;                   -- if no interrupt, IR loads from ProgDB value
                else
                    IR <= OpCALLI;                  -- otherwise do a CALLI op
                    Int_RJMPAddr <= IntEventReg;    -- and latch RJMP register for RJMP address info
                end if;
            else
                IR <= IR;                           -- still doing operation so hold IR
            end if;
        end if;
    end process IR_update;


    -- detects if any interrupt signal goes active during the current operation cycles
    -- upon completion of current instruction, halts operation to execute interrupt
    -- handler function, returning to next location in program memory upon completion
    INT_latch: process (CLK, Reset, INT0, INT1, T1CAP, T1CPA, T1CPB, T1OVF,
                        T0OVF, IRQSPI, UARTRX, UARTRE, UARTTX, ANACMP, SReg, cycle, IR)
    begin
        if Reset = '0' then                         -- if reset, load reset interrupt
            IntEventReg <= RESET_INT;
        
        elsif cycle = ONE_CYCLE and IR = OpCALLI then
            IntEventReg <= NO_INT;                  -- if first cycle of CALLI op, clear interrupt reg
        
        elsif SReg(I_SREG) = INT_EN then            -- if any other interrupt, load with appropriate RJMP addr
            if (INT0 = ACTIVE_LO) then              -- interrupt priority from first to last starting with highest prio
                IntEventReg <= INT0_INT;
            elsif (INT1 = ACTIVE_LO) then
                IntEventReg <= INT1_INT;
            elsif (T1CAP = ACTIVE_HI) then
                IntEventReg <= T1CAP_INT;
            elsif (T1CPA = ACTIVE_HI) then
                IntEventReg <= T1CPA_INT;
            elsif (T1CPB = ACTIVE_HI) then
                IntEventReg <= T1CPB_INT;
            elsif (T1OVF = ACTIVE_HI) then
                IntEventReg <= T1OVF_INT;
            elsif (T0OVF = ACTIVE_HI) then
                IntEventReg <= T0OVF_INT;
            elsif (IRQSPI = ACTIVE_HI) then
                IntEventReg <= IRQSPI_INT;
            elsif (UARTRX = ACTIVE_HI) then
                IntEventReg <= UARTRX_INT;
            elsif (UARTRE = ACTIVE_HI) then
                IntEventReg <= UARTRE_INT;
            elsif (UARTTX = ACTIVE_HI) then
                IntEventReg <= UARTTX_INT;
            elsif (ANACMP = ACTIVE_HI) then
                IntEventReg <= ANACMP_INT;
            else
                IntEventReg <= IntEventReg;
            end if;
        else
            IntEventReg <= NO_INT;
        end if;
    end process INT_latch;



    -- cycle counter, only operates when cycle_num /= 1
    FSM_noSM : process (CLK)
    begin
      if (rising_edge(CLK)) then
            if Reset = '0' then                     -- if reset, on first cycle of op
                cycle <= "00";
            elsif cycle /= cycle_num-1 then         -- unless on last cycle of op, increment cycle
                cycle <= cycle + 1;
            else                                    -- otherwise was on last cycle of op so reset cycle
                cycle <= "00";
            end if;
      end if;
    end process FSM_noSM;

end RISC;
