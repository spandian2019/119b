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
--
----------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

library opcodes; 
use opcodes.opcodes.all; 

library CPUconst;
use CPUconst.constants.all;

entity CU is
    port(
        ProgDB  : in std_logic_vector(15 downto 0); -- program memory data bus
        SReg    : in std_logic_vector(7 downto 0);  -- status flags
        
        -- to registers
        RegWEn  : out std_logic; -- register write enable 
        RegWSel : out std_logic_vector(4 downto 0); -- register write select
        RegSelA : out std_logic_vector(4 downto 0); -- register A select
        RegSelB : out std_logic_vector(4 downto 0); -- register B select

        -- to ALU and SReg
        ALUOp   : out std_logic_vector(4 downto 0); -- operation control signals 
        ALUSel  : out std_logic_vector(2 downto 0); -- operation select 
        FlagMask: out std_logic_vector(7 downto 0); -- mask for writing to flags
        
        -- I/O
        IOEn    : out std_logic; -- enable I/O port operations 
        K       : out std_logic_vector(7 downto 0); -- immediate value K 
        SRInOut : out std_logic; -- in/out control for I/O ports 
        
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

end CU;

--
--  CU Architecture
--

architecture RISC of CU is

    -- FSM States
    type states is (
        Done,
        Count
    );

    signal FSMState : states := Done;

    signal CLK : std_logic;
    
    signal load         :   std_logic;

    signal cycle_num    :   std_logic_vector(1 downto 0);
    signal cycle        :   std_logic_vector(1 downto 0);

    signal IR           :   std_logic_vector(15 downto 0);

begin

    Decoder : process (CLK)
    begin
        if (rising_edge(CLK)) then
            -- sets cycle number for op_codes 
            -- defaults to 1
            cycle_num = "01";
            if (std_match(IR, OpADIW) or 
                std_match(IR, OpADIW) or 
                std_match(IR, OpADIW) or 
                std_match(IR, OpADIW)) then
                    cycle_num = "10";
            end if;

            -- register default values
            K <= IR(11 downto 8) & IR(3 downto 0);
            RegSelA <= IR(8 downto 4);
            RegWEn  <= '1';

            -- not support all MULS operations so can combine
            --  all operations that do single byte 
            if  std_match(IR, "000-------------") then
                ALUSel <= AddSubEn;
                -- ALU Cin = IR(12);
                -- ALU sub flag = [IR(11) xor IR(10)]
                RegSelB <= IR(9) & IR(3..0)
                RegWSel <= IR(8..4) & IR(11)
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;
            if  std_match(IR, OpADIW) then
                slkdj;
            end if;

    
        end if;
    end process Decoder;


    load <= '1' when cycle = cycle_num else
            '0';

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

    ---- handles FSM transitions
    --FSM_trans : process (CLK)
    --begin
    --    if (rising_edge(CLK)) then
    --        case FSMState is
    --            when Done =>
    --                if cycle_num /= cycle then
    --                    FSMState <= Count;
    --                end if;
    --            when Count =>
    --                if cycle_num = cycle then
    --                    FSMState <= Done;
    --                end if;
    --        end case;
    --    end if;
    --end process FSM_trans;

    ---- handles FSM actions
    --FSM_actions : process (CLK)
    --begin
    --    if (rising_edge(CLK)) then
    --        case FSMState is
    --            when Done =>
    --                load <= '1';
    --                cycle = "00";
    --            when Count =>
    --                load <= '0';
    --                cycle <= cycle + 1;
    --        end case;
    --    end if;
    --end process FSM_actions;

end RISC;














