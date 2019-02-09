 ----------------------------------------------------------------------------
--
--
-- Control Unit
--
-- RISC Control Unit for the AVR CPU. This contains the 16-bit instruction
-- register, logic for instruction decoding, and a finite state machine for
-- instruction cycle counts. It outputs all the necessary control signals for
-- executing instructions, including addressing, ALU operations, register
-- operations,
--
-- Ports:
--
-- Revision History:
-- 01/24/2019   Sophia Liu      Initial revision
-- 01/29/2019   Sundar Pandian  Testing Git, branches
-- 01/30/2019   Sundar Pandian  Initial architecture writeup
-- 01/31/2019   Sundar Pandian  added bitmask support
-- 02/01/2019   Sundar Pandian  debugged with testbench support
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
        ProgDB  : in std_logic_vector(ADDRSIZE-1 downto 0);     -- program memory data bus
        IR      : in std_logic_vector(IRSIZE-1 downto 0);     -- instruction register input
        SReg    : in std_logic_vector(REGSIZE-1 downto 0);      -- status flags
        load    : buffer std_logic;                     -- load output to tell IR register
                                                        --  when to fetch new instruction

        Immed       : out std_logic_vector(REGSIZE-1 downto 0);   -- immediate value K
        ImmedEn     : out std_logic;

        -- to register unit
        RegWEn      : out std_logic;                    -- register write enable
        RegWSel     : out std_logic_vector(RADDRSIZE-1 downto 0); -- register write select
        RegSelA     : out std_logic_vector(RADDRSIZE-1 downto 0); -- register A select
        RegSelB     : out std_logic_vector(RADDRSIZE-1 downto 0); -- register B select
        IORegWEn    : out std_logic;                      -- IN command enable
        IORegWSel   : out std_logic_vector(IOADDRSIZE-1 downto 0);   -- IO register address bus
        IndWEn      : out std_logic;
        IndAddrSel  : out ADDR_SEL;
        IOOutSel    : out std_logic;

        DataRd          : out std_logic; -- indicates data memory is being read
        DataWr          : out std_logic; -- indicates data memory is being written

        IORegOutEn  : out   std_logic;                      -- OUT command enable

        -- to ALU and SReg
        ALUaddsub   : out ALU_ADDSUB;
        ALUsr       : out ALU_SR;
        ALUfop      : out ALU_FOPS; -- operation control signals
        ALUcomneg   : out ALU_COMNEG;
        ALUSel      : out ALU_SELECTS; -- operation select
        bitmask     : out BIT_MASK; -- mask for writing to flags (SReg)
        CPC         : out std_logic;

        LoadIn      : out LOADIN_SEL; -- selects data line into reg
        SRegLd      : out   std_logic;                      -- select line to mux status reg source


        ---- Program memory access
        --ProgAddr: out std_logic_vector(15 downto 0); -- address source for program memory unit
        --ProgLoad: out std_logic;                    -- load select for PC
        --ProgAddrSel : in std_logic_vector(1 downto 0);  -- program address source select

        -- Data memory access
        DataAddrSel     : out ADDR_SEL;  -- data address source select
        DataOffsetSel   : out OFFSET_SEL;-- data address offset source select
        PreSel          : out PREPOST_ADDR; -- data pre/post address select
        QOffset         : out std_logic_vector(Q_OFFSET_SIZE-1 downto 0); -- address offset for data memory unit

        ---- Stack
        --StackEn     : out std_logic; -- stack enable signal
        --StackPush   : out std_logic; -- stack push/pop control
        --Reset       : out std_logic; -- active low reset signal

        CLK         : in    std_logic                       -- system clock
    );

end CU;

--
--  CU Architecture
--

architecture RISC of CU is
    signal cycle_num    :   OP_CYCLE := ZERO_CYCLES;
    signal cycle        :   std_logic_vector(1 downto 0) := "00";
begin

    -- asynchronously decodes IR inputs
    decoder : process(IR, CLK)
    begin
            -- sets cycle number for op_codes
            -- defaults operations to 1 cycle
            cycle_num <= ONE_CYCLE;
            -- control signals default values, reset all
            -- this way each operation only enables actions
            RegWEn      <= WRITE_DIS;
            IORegWEn    <= WRITE_DIS;
            IndWEn      <= WRITE_DIS;
            ImmedEn     <= IMM_DIS;
            bitmask     <= MASK_NONE;
            CPC         <= CPC_RST;
            -- set F Block to pass through B Register Value
            ALUfop      <= FOP_B;
            -- set COMNEG block to pass through B Register Value
            ALUcomneg   <= COMNEG_NONE;
            -- default writing outputs from Reg A back into Reg space
            LoadIn <= LD_REGA;

      --      -- considering single byte adder/subber ops:
      --      -- ADC, ADD, SBC, SUB, CPC, CP
      --      -- all of these have the top three bits in IR cleared
      --      if  std_match(IR, "000-------------") then
      --      --  000ooordddddrrrr  -- IR
      --          RegSelA <= IR(8 downto 4);
      --          RegSelB <= IR(9) & IR(3 downto 0);
      --          RegWSel <= IR(8 downto 4);
      --          BitMask <= MASK_ADD;

      --          -- enable Adder/Subber output
      --          ALUSel <= ADDSUBOUT;
      --          -- write enable encoded in IR
      --          -- CP and CPC doesn't rewrite register value, mapped in IR
      --          RegWEn <= IR(11);
      --          -- subber flag mapped in IR as function of 2 bits
      --          ALUaddsub(subFlag)  <= IR(11) xor IR(10);
      --          -- carry/nborrow bit mapped in IR
      --          if (IR(12) xor IR(11) xor IR(10)) = '1' then
      --              -- send carry bit to ALU
      --              ALUaddsub(CARRY_S1 downto CARRY_S0) <= CARRY_IN;
      --          else
      --              -- all no carry operations use same logic block with 
      --              -- carry in mapped in IR
      --              --  clear active-hi carry for add
      --              --  clear active-lo borrow for sub
      --              -- maps same as to subFlag
      --              if IR(11) xor IR(10) = '1' then
      --                  ALUaddsub(CARRY_S1 downto CARRY_S0) <= SET_CARRY;
      --              else
      --                  ALUaddsub(CARRY_S1 downto CARRY_S0) <= RST_CARRY;
      --              end if;
      --          end if;
      --      end if;

      --      -- considering word adder/subber ops
      --      if  std_match(IR, OpADIW) or std_match(IR, OpSBIW) then
      --      --  1001011oKKddKKKK  -- IR
      --          RegSelA <= IR(8 downto 4);
      --          RegSelB <= IR(9) & IR(3 downto 0);
      --          RegWSel <= IR(8 downto 4);
      --          BitMask <= MASK_ADIW;

      --          -- takes 2 cycles to complete operation
      --          cycle_num <= TWO_CYCLES;
      --          -- enable Adder/Subber operation
      --          ALUSel <= ADDSUBOUT;
      --          -- subFlag mapped in IR
      --          ALUaddsub(subFlag) <= IR(8);
      --          -- immediate value loads into second operand
      --          LoadReg <= LoadB;

      --          -- value in IR is offset added to register 24
      --          --  possible operands include {24, 26, 28, 30}
      --          --  low byte operation uses above bytes while high byte
      --          --    operation uses the next highest byte

      --          -- if just loaded IR, doing first cycle
      --          if load = '1' then
      --              -- mapping to immediate value in IR, max value of 63
      --              K <= "00" & IR(7 downto 6) & IR(3 downto 0);
      --              -- add/sub op mapped in 
      --              ALUaddsub(subFlag)  <= IR(8);
      --              -- carry/nborrow cleared
      --              if IR(8) = '1' then
      --                  ALUaddsub(CARRY_S1 downto CARRY_S0) <= SET_CARRY;
      --              else
      --                  ALUaddsub(CARRY_S1 downto CARRY_S0) <= RST_CARRY;
      --              end if;
      --              -- limits operand addresses
      --              RegSelA <= "11" & IR(5 downto 4) & '0';
      --              RegWSel <= "11" & IR(5 downto 4) & '0';
      --          elsif load = '0' then
      --              -- add in 0
      --              K <= (others => '0');
      --              -- carry out from low byte carries in to high byte add
      --              ALUaddsub(CARRY_S1 downto CARRY_S0) <= CARRY_IN;
      --              -- previous operand addresses + 1
      --              RegSelA <= "11" & IR(5 downto 4) & '1';
      --              RegWSel <= "11" & IR(5 downto 4) & '1';
      --          end if;
                
      --      end if;

      --      -- considering word multiply op
      --      if  std_match(IR, OpMUL) then
      --          -- takes 2 cycles to complete operation
      --          cycle_num <= "10";
      --          -- enable MUL operation
      --          ALUSel <= MulEn;

      --          -- output of MUL op is saved in R1:R0
      --          --  low byte operation uses above bytes while high byte
      --          --    operation uses the next highest byte

      --          RegSelB <= IR(9) & IR(3 downto 0);
      --          -- first do low byte multiply
      --          if cycle = "00" then
      --              RegWSel <= "00000";
      --          elsif cycle = "01" then
      --              RegWSel <= "00001";
      --          end if;
                
      --          BitMask <= MASK_MUL;
      --      end if;

      --      -- considering immediate subber operations
      --      if  std_match(IR, OpSUBI) or std_match(IR, OpSBCI) or std_match(IR, OpCPI) then
      --          -- enable Adder/Subber operation
      --          ALUSel <= AddSubEn;
      --          -- carry/nborrow bit mapped in IR
      --          if IR(12) = '1' then
      --              -- send carry bit to ALU
      --              ALUaddsub(CARRY_S1 downto CARRY_S0) <= CARRY_IN;
      --          else
      --              -- all no carry operations use same logic block with 
      --              -- carry in mapped in IR
      --              --  clear active-hi carry for add
      --              --  clear active-lo borrow for sub
      --              -- maps same as to subFlag
      --              ALUaddsub(CARRY_S1 downto CARRY_S0) <= SET_CARRY;
      --          end if;
      --          -- subbing so subFlag active
      --          ALUaddsub(subFlag)  <= '1';
      --          -- CPI doesn't rewrite register, mapped in IR
      --          RegWEn <= IR(14);
      --          -- immediate value loads into second operand
      --          LoadReg <= LoadB;
      --          RegSelA <= '1' & IR(7 downto 4);
      --          RegWSel <= '1' & IR(7 downto 4);
      --          BitMask <= MASK_ADD;
      --      end if;

      --      -- considering incrementing/decrementing operations
      --      if  std_match(IR, OpINC) or std_match(IR, OpDEC) then
      --          -- enable Adder/Subber operation
      --          ALUSel <= AddSubEn;
      --          -- carry in mapped in IR
      --          --  clear active-hi carry for add
      --          --  clear active-lo borrow for sub
      --          -- maps same as to subFlag
      --          if IR(3) = '1' then
      --              ALUaddsub(CARRY_S1 downto CARRY_S0) <= SET_CARRY;
      --          else
      --              ALUaddsub(CARRY_S1 downto CARRY_S0) <= RST_CARRY;
      --          end if;
      --          -- add/sub conditional mapped in IR
      --          ALUaddsub(subFlag)  <= IR(3);
      --          K <= "00000001";
      --          -- immediate value loads into second operand
      --          LoadReg <= LoadB;
      --          RegWSel <= IR(8 downto 4);
      --          BitMask <= MASK_DECINC;
      --      end if;

      --      -- considering COM and NEG operations
      --      if  std_match(IR, OpCOM) or std_match(IR, OpNEG) then
      --          -- enable Adder/Subber operation
      --          ALUSel <= AddSubEn;
      --          -- carry in mapped in IR
      --          --  clear active-hi carry for add
      --          --  clear active-lo borrow for sub
      --          -- clear nborrow
      --          ALUaddsub(CARRY_S1 downto CARRY_S0) <= SET_CARRY;
      --          -- always subbing so set
      --          ALUaddsub(subFlag)  <= '1';
      --          -- either subtract operand from xFF or x00
      --          -- xFF for NEG
      --          -- x00 for COM
      --          K <= (others => IR(0));
      --          -- immediate value loads into first operand
      --          LoadReg <= LoadA;
      --          -- data into register value from register A output
      --          LoadIn  <= LdRegA;
      --          RegWSel <= IR(8 downto 4);
      --          -- set bitmask based on if COM op or NEG op
      --          if IR(0) = '0' then
      --              BitMask <= MASK_COM;
      --          else
      --              BitMask <= MASK_NEG;
      --          end if;
      --      end if;

      --      if  std_match(IR, OpAND) or std_match(IR, OpANDI) then
      --          -- enable F Block Operation
      --          ALUSel <= FBlockEn;
      --          -- select AND operation
      --          ALUfop <= OP_AND;
      --          if IR(14) = '1' then
      --              -- immediate value loads into second operand if ANDI op
      --              LoadReg <= LoadB;
      --          end if;
      --          RegWSel <= IR(8 downto 4);
      --          -- ANDI operation only maps to upper half of register space
      --          if IR(14) = '1' then
      --              RegSelA(4) <= '1';
      --              RegWSel(4) <= '1';
      --          end if;
      --          RegSelB <= IR(9) & IR(3 downto 0);
      --          BitMask <= MASK_ANDOR;
      --      end if;

      --      if  std_match(IR, OpOR) or std_match(IR, OpORI) then
      --          -- enable F Block Operation
      --          ALUSel <= FBlockEn;
      --          -- select OR operation
      --          ALUfop <= OP_OR;
      --          if IR(14) = '1' then
      --              -- immediate value loads into second operand
      --              LoadReg <= LoadB;
      --          end if;
      --          RegWSel <= IR(8 downto 4);
      --          -- ORI operation only maps to upper half of register space
      --          if IR(14) = '1' then
      --              RegSelA(4) <= '1';
      --              RegWSel(4) <= '1';
      --          end if;
      --          RegSelB <= IR(9) & IR(3 downto 0);
      --          BitMask <= MASK_ANDOR;
      --      end if;

      --      if  std_match(IR, OpEOR) then
      --          -- enable F Block Operation
      --          ALUSel <= FBlockEn;
      --          -- select XOR operation
      --          ALUfop <= OP_XOR;
      --          RegWSel <= IR(8 downto 4);
      --          RegSelB <= IR(9) & IR(3 downto 0);
      --          BitMask <= MASK_EOR;
      --      end if;

      --      if  std_match(IR, OpLSR) then
      --          -- enable shifter/rotator operation
      --          ALUSel <= ShiftEn;
      --          -- select LSR operation
      --          ALUsr <= OP_LSR;
      --          RegWSel <= IR(8 downto 4);
      --          BitMask <= MASK_SHIFT;
      --      end if;

      --      if  std_match(IR, OpASR) then
      --          -- enable shifter/rotator operation
      --          ALUSel <= ShiftEn;
      --          -- select LSR operation
      --          ALUsr <= OP_ASR;
      --          RegWSel <= IR(8 downto 4);
      --          BitMask <= MASK_SHIFT;
      --      end if;

      --      if  std_match(IR, OpROR) then
      --          -- enable shifter/rotator operation
      --          ALUSel <= ShiftEn;
      --          -- select LSR operation
      --          ALUOp <= OP_ROR;
      --          -- ROR op uses carry bit from last operation ----------- TODO
      --          ALUsr(CARRY_S1 downto CARRY_S0) <= CARRY_IN;
      --          RegWSel <= IR(8 downto 4);
      --          BitMask <= MASK_SHIFT;
      --      end if;

      --      if  std_match(IR, OpBCLR) or std_match(IR, OpBSET) then
      --          -- status register source is Control Unit
      --          SRegLd <= LdSRCtrlU;
      --          -- set or reset all status register outputs
      --          SRegOut <= (others => not IR(7));
      --          -- specific bit to be cleared/set uses bitmask
      --          --  clear bitmask
      --          bitmask <= (others => '0');
      --          --  then set proper bit high in bitmask
      --          bitmask(conv_integer(IR(6 downto 4))) <= '1';
      --      end if;

      --      if  std_match(IR, OpBLD) or std_match(IR, OpBST) then
					 --ALUOp(0) <= SReg(6); -- send transfer bit to ALU
      --          ALUSel <= PassThruEn;
      --          RegWSel <= IR(8 downto 4);
      --          -- clear bitmask
      --          BitMask <= (others => '0');
      --          -- store/loads T bit
      --          BitMask(T_SREG) <= IR(T_IR);
      --      end if;

      --      if std_match(IR, OpSWAP) then
      --          -- register array handles nibble swapping
      --          LoadReg <= LoadSwap;
      --          LoadIn <= LdRegA;
      --          RegWSel <= IR(8 downto 4);
      --          BitMask <= MASK_NONE;
      --      end if;

      --      if std_match(IR, OpIN) or std_match(IR, OpOUT) then
      --          -- not done
      --          RegWSel <= IR(8 downto 4);
      --          RegWEn     <= IR(11);
      --          IORegInEn  <= not IR(11);
      --          IORegOutEn <= IR(11);
      --          LoadIn <= LdIO;
      --      end if;

            if  std_match(IR, OpLDX) or
                std_match(IR, OpLDXI) or
                std_match(IR, OpLDXD) or
                std_match(IR, OpLDYI) or
                std_match(IR, OpLDYD) or
                std_match(IR, OpLDZI) or
                std_match(IR, OpLDZD) then
                -- 1001000dddddoooo
                    -- takes 2 cycles to complete operation
                    cycle_num <= TWO_CYCLES;
                    -- loading values into register space from DataDB
                    LoadIn <= LD_DB;
                    -- offset values for 0, +1, -1 stored in low two bits of IR
                    -- add  0 -> "00" = ZERO_SEL
                    -- add +1 -> "01" = INC_SEL
                    -- add -1 -> "10" = DEC_SEL
                    DataOffsetSel <= IR(1 downto 0);
                    -- pre flag setting stored in IR(1)
                    -- pre-op -> IR(1) = '1' = PRE_ADDR
                    -- pre-op -> IR(0) = '0' = POST_ADDR
                    PreSel <= IR(1);
                    -- indirect addressing stored in IR(3..2)
                    -- X -> IR(3..2) = "11" = X_SEL
                    -- Y -> IR(3..2) = "10" = Y_SEL
                    -- Z -> IR(3..2) = "00" = Z_SEL
                    DataAddrSel <= IR(3 downto 2);
                    -- Operand 1 is the register being written to, loc in IR(8..4)
                    RegWSel <= IR(8 downto 4);
                    -- during first cycle
                    if load = '0' then
                        -- do nothing
                    else
                        -- DataRd = CLK for the second cycle in Ld operations
                        DataRd <= CLK;
                        RegWEn <= WRITE_EN;
                        -- RegIn into register needs to be DataDB here
                    end if;
            end if;

            if  std_match(IR, OpLDD) then
                -- 10q0qq0ddddd0qqq
                    -- takes 2 cycles to complete operation
                    cycle_num <= TWO_CYCLES;
                    -- loading values into register space from DataDB
                    LoadIn <= LD_DB;
                    -- offset values for 0, +1, -1 stored in low two bits of IR
                    -- add  0 -> "00" = ZERO_SEL
                    -- add +1 -> "01" = INC_SEL
                    -- add -1 -> "10" = DEC_SEL
                    DataOffsetSel <= IR(1 downto 0);
                    -- pre flag setting stored in IR(1)
                    -- pre-op -> IR(1) = '1' = PRE_ADDR
                    -- pre-op -> IR(0) = '0' = POST_ADDR
                    PreSel <= IR(1);
                    -- indirect addressing stored in IR(3..2)
                    -- X -> IR(3..2) = "11" = X_SEL
                    -- Y -> IR(3..2) = "10" = Y_SEL
                    -- Z -> IR(3..2) = "00" = Z_SEL
                    DataAddrSel <= IR(3 downto 2);
                    -- Operand 1 is the register being written to, loc in IR(8..4)
                    RegWSel <= IR(8 downto 4);
                    -- during first cycle
                    if load = '0' then
                        -- do nothing
                    else
                        -- DataRd = CLK for the second cycle in Ld operations
                        DataRd <= CLK;
                        RegWEn <= WRITE_EN;
                        -- RegIn into register needs to be DataDB here
                    end if;
            end if;

            if  std_match(IR, OpLDI) then
                -- 1110kkkkddddkkkk
                    -- takes 1 cycle to complete operation
                    cycle_num <= ONE_CYCLE;
                    -- loading values into register space from DataDB
                    LoadIn <= LD_IMM;
                    -- Operand 1 is the register being written to
                    -- Immediate operations limited to upper half of register space
                    -- so, MSB of RADDRSIZE = '1' and rest is loc in IR(7..4)
                    RegWSel <= '1' & IR(7 downto 4);
                    -- immediate value found in k locs in IR, IR(11..8)&IR(3..0)
                    Immed <= IR(11 downto 8) & IR(3 downto 0);
                    RegWEn <= '1';
            end if;


    end process decoder;

    -- load enable signal telling when to fetch next instruction
    load <= '1' when cycle = cycle_num-1 else
            '0';

    -- cycle counter, only operates when cycle_num /= 1
    FSM_noSM : process (CLK)
    begin
      if (rising_edge(CLK)) then
            if load = '0' then
                cycle <= cycle + 1;
            else
                cycle <= "00";
            end if;
      end if;
    end process FSM_noSM;

end RISC;














