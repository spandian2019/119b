----------------------------------------------------------------------------
--
--  Atmel AVR Program Memory
--
--  This component describes a program for the AVR CPU.  It creates the
--  program in a small (334 x 16) ROM.
--
--  Revision History:
--     11 May 00  Glen George       Initial revision (from 5/9/00 version of
--                                  progmem.vhd).
--     28 Jul 00  Glen George       Added instructions and made memory return
--                                  NOP when not mapped.
--      7 Jun 02  Glen George       Updated commenting.
--     16 May 04  Glen George       Added more instructions for testing and
--                                  updated commenting.
--     21 Jan 08  Glen George       Updated commenting.
--     17 Jan 18  Glen George       Updated commenting.
--     25 Feb 19  Sophia Liu        Changed sample program
--
----------------------------------------------------------------------------


--
--  PROG_MEMORY
--
--  This is the program memory component.  It is just a 334 word ROM with no
--  timing information.  It is meant to be connected to the AVR CPU.  The ROM
--  is always enabled and may be changed when Reset it active.
--
--  Inputs:
--    ProgAB - address bus (16 bits)
--    Reset  - system reset (active low)
--
--  Outputs:
--    ProgDB - program memory data bus (16 bits)
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;


entity  PROG_MEMORY  is

    port (
        ProgAB  :  in   std_logic_vector(15 downto 0);  -- program address bus
        Reset   :  in   std_logic;                      -- system reset
        ProgDB  :  out  std_logic_vector(15 downto 0)   -- program data bus
    );

end  PROG_MEMORY;


architecture  ROM  of  PROG_MEMORY  is

    -- define the type for the ROM (an array)
    type  ROMtype  is array(0 to 678) of std_logic_vector(15 downto 0);

    -- define the actual ROM to test program 
    signal  ROMbits  :  ROMtype  :=  ( X"E000", X"E010", X"E020", X"E030", X"E040",
                                        X"2EF0", X"2E10", X"2E20", X"2E30", X"2E40",
                                        X"2E50", X"2E60", X"2E70", X"2E80", X"2E90",
                                        X"2EA0", X"2EB0", X"2EC0", X"2ED0", X"2EE0",
                                        X"C000", X"2411", X"E080", X"E090", X"BE1F",
                                        X"9641", X"B63F", X"9230", X"FE00", X"3090",
                                        X"F409", X"0000", X"3181", X"F409", X"0000",
                                        X"EF00", X"E512", X"0F01", X"B72F", X"9320",
                                        X"FE00", X"3402", X"F409", X"0000", X"E020",
                                        X"E000", X"0F02", X"B63F", X"9230", X"FE01",
                                        X"3000", X"F409", X"0000", X"E00F", X"E811",
                                        X"1F01", X"B63F", X"9230", X"FE02", X"3900",
                                        X"F409", X"0000", X"E820", X"E800", X"E410",
                                        X"E430", X"1F02", X"B63F", X"9230", X"FE03",
                                        X"0F13", X"B63F", X"9230", X"FE00", X"3000",
                                        X"F409", X"0000", X"3810", X"F409", X"0000",
                                        X"E000", X"BF0F", X"ED08", X"ED13", X"2301",
                                        X"B63F", X"9230", X"FF00", X"3D00", X"F409",
                                        X"0000", X"E21F", X"2301", X"B63F", X"9230",
                                        X"FF00", X"3000", X"F409", X"0000", X"ED08",
                                        X"7D03", X"B63F", X"9230", X"FF00", X"3D00",
                                        X"F409", X"0000", X"720F", X"B63F", X"9230",
                                        X"FF00", X"3000", X"F409", X"0000", X"E801",
                                        X"9505", X"B63F", X"9230", X"FF00", X"3C00",
                                        X"F409", X"0000", X"E801", X"9506", X"B63F",
                                        X"9230", X"FF00", X"3400", X"F409", X"0000",
                                        X"E109", X"BF0F", X"9498", X"B63F", X"9230",
                                        X"FF00", X"9488", X"B63F", X"9230", X"0070",
                                        X"94C8", X"B63F", X"9230", X"0071", X"9418",
                                        X"B63F", X"9230", X"FF00", X"94F8", X"B63F",
                                        X"9230", X"FF80", X"9468", X"B63F", X"9230",
                                        X"FF81", X"E001", X"F907", X"B63F", X"9230",
                                        X"FF00", X"3801", X"F409", X"0000", X"FB01",
                                        X"B63F", X"9230", X"FF00", X"3801", X"F409",
                                        X"0000", X"9503", X"3802", X"F409", X"0000",
                                        X"950A", X"3801", X"F409", X"0000", X"EA0F",
                                        X"E71F", X"1B01", X"B63F", X"9230", X"FF00",
                                        X"3300", X"F409", X"0000", X"5300", X"B63F",
                                        X"9230", X"FF00", X"3000", X"F409", X"0000",
                                        X"9502", X"3000", X"F409", X"0000", X"E800",
                                        X"9502", X"3008", X"F409", X"0000", X"E000",
                                        X"9500", X"EF1F", X"1301", X"0000", X"B78F",
                                        X"E195", X"1389", X"0000", X"9500", X"E010",
                                        X"1301", X"0000", X"B78F", X"E093", X"1389",
                                        X"0000", X"E506", X"9500", X"EA19", X"1301",
                                        X"0000", X"B78F", X"E195", X"1389", X"0000",
                                        X"E505", X"EA1A", X"2701", X"B78F", X"E195",
                                        X"1389", X"0F01", X"0000", X"3F0F", X"F009",
                                        X"E000", X"BE0F", X"EF1F", X"2701", X"B78F",
                                        X"E092", X"1389", X"0000", X"3000", X"F011",
                                        X"9300", X"FF00", X"EF1F", X"9511", X"B78F",
                                        X"E291", X"1389", X"0000", X"3011", X"F409",
                                        X"0000", X"E010", X"9511", X"B78F", X"E092",
                                        X"1389", X"0000", X"3010", X"F409", X"0000",
                                        X"E810", X"9511", X"B78F", X"E09D", X"1389",
                                        X"0000", X"3810", X"F409", X"0000", X"E505",
                                        X"EA1A", X"2B01", X"B78F", X"E194", X"1389",
                                        X"0000", X"3F0F", X"F409", X"0000", X"6212",
                                        X"B78F", X"E194", X"1389", X"0000", X"3A1A",
                                        X"F409", X"0000", X"BE0F", X"9408", X"9517",
                                        X"B78F", X"E09C", X"1389", X"0000", X"3D15",
                                        X"F409", X"0000", X"BE0F", X"9488", X"9517",
                                        X"B78F", X"E199", X"1389", X"0000", X"361A",
                                        X"F409", X"0000", X"BE0F", X"9488", X"E000",
                                        X"EF1F", X"0B01", X"B78F", X"E291", X"1389",
                                        X"0000", X"3001", X"F409", X"0000", X"BE0F",
                                        X"9408", X"E500", X"E710", X"0B01", X"B78F",
                                        X"E395", X"1389", X"0000", X"3D0F", X"F409",
                                        X"0000", X"BE0F", X"9408", X"E71A", X"471A",
                                        X"B78F", X"E395", X"1389", X"0000", X"3F1F",
                                        X"F409", X"0000", X"BE0F", X"E78A", X"E091",
                                        X"970A", X"B70F", X"E010", X"1301", X"0000",
                                        X"9408", X"E700", X"E011", X"1780", X"0791",
                                        X"F409", X"0000", X"E101", X"FF00", X"EF0F",
                                        X"FD02", X"0F01", X"FD05", X"9641", X"B78F",
                                        X"EA0B", X"B79F", X"1789", X"F409", X"0000",
                                        X"3A0B", X"F409", X"0000", X"E011", X"9310",
                                        X"FF23", X"EFBF", X"E2A3", X"B78F", X"910C",
                                        X"B79F", X"1789", X"F409", X"0000", X"3001",
                                        X"F409", X"0000", X"B78F", X"911D", X"B79F",
                                        X"1789", X"F409", X"0000", X"1710", X"F409",
                                        X"0000", X"E212", X"9310", X"FF24", X"912C",
                                        X"3222", X"F409", X"0000", X"B78F", X"913E",
                                        X"B79F", X"1789", X"F409", X"0000", X"1730",
                                        X"F409", X"0000", X"EE0E", X"9300", X"FF45",
                                        X"EFDF", X"E4C5", X"9119", X"3E1E", X"F409",
                                        X"0000", X"34C6", X"F409", X"0000", X"913A",
                                        X"1730", X"F409", X"0000", X"E708", X"9300",
                                        X"FEA1", X"EFFE", X"EAE1", X"9111", X"1710",
                                        X"F409", X"0000", X"3AE2", X"F409", X"0000",
                                        X"9132", X"1730", X"F409", X"0000", X"E026",
                                        X"9320", X"0076", X"27DD", X"E7C1", X"B78F",
                                        X"810D", X"B79F", X"1789", X"F409", X"0000",
                                        X"3006", X"F409", X"0000", X"EF2F", X"9320",
                                        X"0023", X"27FF", X"E2E2", X"B78F", X"8101",
                                        X"B79F", X"1789", X"F409", X"0000", X"3F0F",
                                        X"F409", X"0000", X"E22B", X"9320", X"FF81",
                                        X"9140", X"FF81", X"324B", X"F409", X"0000",
                                        X"E246", X"E950", X"B78F", X"2F54", X"B79F",
                                        X"1789", X"F409", X"0000", X"1754", X"F409",
                                        X"0000", X"3256", X"F409", X"0000", X"E568",
                                        X"27BB", X"E7AB", X"B78F", X"936C", X"B79F",
                                        X"1789", X"F409", X"0000", X"917C", X"1776",
                                        X"F409", X"0000", X"E102", X"930D", X"E72C",
                                        X"E030", X"17A2", X"07B3", X"F409", X"0000",
                                        X"911E", X"3112", X"F409", X"0000", X"E424",
                                        X"932E", X"E72A", X"E030", X"17A2", X"07B3",
                                        X"F409", X"0000", X"913C", X"3434", X"F409",
                                        X"0000", X"27DD", X"E1C9", X"E102", X"9309",
                                        X"E12A", X"E030", X"17C2", X"07D3", X"F409",
                                        X"0000", X"911A", X"3112", X"F409", X"0000",
                                        X"E424", X"932A", X"E128", X"E030", X"17C2",
                                        X"07D3", X"F409", X"0000", X"9139", X"3434",
                                        X"F409", X"0000", X"27FF", X"E6E9", X"E203",
                                        X"9301", X"E62A", X"E030", X"17E2", X"07F3",
                                        X"F409", X"0000", X"9112", X"3213", X"F409",
                                        X"0000", X"E424", X"9322", X"E628", X"E030",
                                        X"17E2", X"07F3", X"F409", X"0000", X"9131",
                                        X"3434", X"F409", X"0000", X"EFDF", X"E7C7",
                                        X"E743", X"8B49", X"8959", X"1745", X"F409",
                                        X"0000", X"2755", X"E6E7", X"EA4A", X"8B41",
                                        X"8951", X"1745", X"F409", X"0000", X"E909",
                                        X"9300", X"FE57", X"9110", X"FE57", X"1701",
                                        X"F409", X"0000", X"938F", X"930f", X"938F",
                                        X"E520", X"B78F", X"932F", X"B79F", X"1789",
                                        X"F409", X"0000", X"EE3F", X"933F", X"E331",
                                        X"E627", X"B78F", X"913F", X"B79F", X"1789",
                                        X"F409", X"0000", X"3E3F", X"F409", X"0000",
                                        X"912F", X"3520", X"F409", X"0000", X"B78F",
                                        X"C002", X"0000", X"0000", X"B79F", X"1789",
                                        X"F409", X"0000", X"B78F", X"D013", X"B79F",
                                        X"1389", X"0000", X"D00F", X"C002", X"0000",
                                        X"0000", X"EAE1", X"E0F2", X"B78F", X"9509",
                                        X"B79F", X"1389", X"0000", X"9A93", X"9893",
                                        X"0000", X"9508", X"0000", X"0000", X"2D00",
                                        X"9508", X"0000", X"0C12", X"9508");


begin


    -- always read the value at the current address
    ProgDB <= ROMbits(CONV_INTEGER(ProgAB)) when (CONV_INTEGER(ProgAB) <= ROMbits'high)  else
              X"E0C0";    -- NOP instruction


    -- process to handle Reset
    process(Reset)
    begin

        -- check if Reset is low now
        if  (Reset = '0')  then
            -- reset is active - initialize the ROM (nothing for now)
        end if;

    end process;

end  ROM;
