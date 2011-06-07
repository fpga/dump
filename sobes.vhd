--
--
-- 1.              _______
--     in_clk----->|Block|
--     in_req----->|  mem|------>out_data
--     input_data->   ^
--                    |
--                 in_mem_v  

-- outerspace
-- 
-- 2. 
--              __________
--  in_clk----->| Filter |
--  in_impulse->| <3 ----X
--              | >=3----|------>out_impulse
--

-- outerspace
--
-- 3.
--            _____________   
-- in_clk---->| Traffic_l |
--            |  alg|-----|----->out_red
--    generic>|delay|-----|----->out_yelow
--            |     |-----|----->out_green



library ieee;
use ieee.std_logic_1164.all;
--library synplify;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_TEXTIO.ALL;
use IEEE.STD_TEXTIO.All;

-- 1.

entity mem_block is
   port   (
            -- interface
            clk_i      : in  std_logic;  -- clock input
            req_i      : in  std_logic ; -- request input
            wen_i      : in std_logic; -- write to ram enable
            rst_i      : in  std_logic ; -- reset
            data_o     : in std_logic_vector(7 downto 0); --data out (from memory block)
            data_i     : out std_logic_vector (7 downto 0) -- data in (fom external file)
               );
end mem_block;

architecture Behavioral of mem_block is

        type TRam is array (0 to 1023) of std_logic_vector (7 downto 0);

        -- заполнение памяти из файла
        impure function init_bram (ram_file_name : in string) return TRam is
                file ramfile : text is in ram_file_name;
                variable line_read : line;
                variable ram_to_return : TRam;
        begin
                FOR i in TRam'range loop
                        readline(ramfile, line_read);
                        read(line_read, ram_to_return(i));
                end loop
                return ram_to_return;
        end function;

        signal Ram : TRam := init_bram("bram1.dat");

begin 
        process (
                clk_i, 
                rst_i, 
                req_i
        )
        begin 
                if (rst_i = '1') then
                        data_o <='0';
                        elsif clk_i'event and clk_i = '1' then
                          if wen_i ='1' then
                                Ram(conv_integer(addr_i)) <= data_i;
                          elsif req_i = '1' then
                                data_o <= Ram (conv_integer(addr_i));
                          else data_o <= '0';
                          end if;
                end if;
        end process;
end Behavioral;
 

--component ram is
--    port (d_i : in std_logic_vector(7 downto 0);
--     addr_i : in  std_logic_vector(2 downto 0);
--    we_i : in  std_logic;
--     clk_i : in  std_logic;
--     ram_o : out std_logic_vector(7 downto 0));
--end ram;

--architecture rtl of ram is
--type mem_type is array (127 downto 0) of std_logic_vector (7
--    downto 0);
--signal mem : mem_type; -- mem is the signal that defines the RAM
--attribute syn_ramstyle : string;
--attribute syn_ramstyle of mem : signal is "block_ram";
--end rtl;
 
--begin

--DataOutA <= RAM(conv_integer(AddrA));
--DataOutB <= RAM(conv_integer(AddrB));

-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
-- 2.

entity filter_block is
           port   (
                 -- interface
                  clk_i      : in  std_logic;  -- clock input
                  rst_i      : in std_logic; -- reset
                  impulse_i  : in  std_logic ; -- impulse in
                  impulse_o  : out  std_logic ; -- impulse out
               );
end filter_block;

architecture filter of filter_block is
        process ( 
                clk_i, 
                rst_i, 
                impulse_i
        )
        begin
                if (rst_i = '1') then
                        data_o <='0';
                        elsif clk_i'event and clk_i = '1' then
                          if impulse_i ='1' then none end if;
                          if impulse_i='1' then none end if;
                          if impulse_i='1' then none end if;
                          if impulse = '1' then impulse_o <= impulse_i; end if; 

        end process;
        end filter;
        



-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
-- 3.

entity traffic_light is
        generic( light_delay:integer:=1;
               ); 
        port (
                clk_i : in std_logic -- clock input
                rst_i : in std_logic -- reset input
                red_o : out std_logic -- red light out
                yellow_o : out std_logic -- yellow light out
                green_o : out std_logic -- green light out
                -- light_o : out std_logic_vector(2 downto 0)
end traffic_light;

architecture traffic of traffic_loght is
        process (clk_i, rst_i, light_delay)
        begin

        end process;

end traffic;





--	memory1: memory
--	port map (
--				clock => clk_i,
--			    request => req_i,
 --               reset => rst_i,
  --              data_out => data_o
--	);

--        entity ramNxM is
--    generic(    AddrWidht : integer := 4;
--        DataWidht : integer := 2);
--   port (
--                clk        : in    std_logic;
--               we        : in    std_logic;
--                AddrA        : in    std_logic_vector(AddrWidht-1  downto 0);
--               DataIn        : in    std_logic_vector(DataWidht-1 downto 0);
--                AddrB        : in    std_logic_vector(AddrWidht-1  downto 0);
--               DataOutA    : out    std_logic_vector(DataWidht-1 downto 0);
--               DataOutB    : out    std_logic_vector(DataWidht-1 downto 0)
--            );
-- end ramNxM;

--architecture Behavioral of ramNxM is
--
--    type ram_type is array ((2**AddrWidht - 1) downto 0) of std_logic_vector (DataWidht-1 downto 0);
--    signal    RAM    :    ram_type;
--
--begin
--
--process (clk)
--begin
--    if rising_edge(Clk) then
--        if we = '1' then
--            RAM(conv_integer(AddrA)) <= DataIn;
--        end if;
--    end if;
--end process;

-- Block memory


--
-- файл bram.dat
-- cодержит строки вида:
-- 00000001
-- 00000010
-- и т.д.


