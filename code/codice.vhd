library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity project_reti_logiche is
    Port ( 
        i_clk : in std_logic;
        i_rst : in std_logic;
        i_start : in std_logic;
        i_add : in std_logic_vector(15 downto 0);
        i_k : in std_logic_vector(9 downto 0);
        
        o_done : out std_logic;
        
        o_mem_addr : out std_logic_vector(15 downto 0);
        i_mem_data : in std_logic_vector(7 downto 0);
        o_mem_data : out std_logic_vector(7 downto 0);
        o_mem_we : out std_logic;
        o_mem_en : out std_logic
    );
end project_reti_logiche;

architecture project_reti_logiche_arch of project_reti_logiche is

component parallel_reg_8 is
    Port ( 
        input : in std_logic_vector (7 downto 0);
        output : out std_logic_vector (7 downto 0);
        enable : in std_logic;
        i_clk : in std_logic;
        i_rst : in std_logic;
        i_done : in std_logic
    );
end component parallel_reg_8;

component address_handler is 
    Port ( 
        i_en : in std_logic;
        i_clk : in std_logic;
        i_rst : in std_logic;
        i_add:  in std_logic_vector(15 downto 0);
        i_done: in std_logic;
        
        o_addr : out std_logic_vector(15 downto 0);
        o_count : out std_logic_vector(10 downto 0)
    ); 
end component address_handler;

component data_handler is 
    Port ( 
        i_en : in std_logic;
        i_clk : in std_logic;
        i_rst : in std_logic;
        i_new_data: in std_logic_vector(7 downto 0);
        i_done: in std_logic;
        
        o_data : out std_logic_vector(7 downto 0);
        o_credibility : out std_logic_vector(4 downto 0) 
    );  
end component data_handler;

component output_handler is 
    Port ( 
        i_en : in std_logic;
        i_sel : in std_logic;
        i_count : in std_logic_vector(10 downto 0);
        i_k : in std_logic_vector(9 downto 0);
        i_data: in std_logic_vector(7 downto 0);
        i_credibility: in std_logic_vector(4 downto 0);
        
        o_output: out std_logic_vector(7 downto 0)
    ); 
end component output_handler;

component shift_reg_1 is 
    Port ( 
        enable : in std_logic;
        output : out std_logic;
        value : out std_logic;
        i_start : in std_logic;
        i_clk : in std_logic;
        i_rst : in std_logic
    );
end component shift_reg_1;

component fsm is
    Port ( 
        i_clk   : in std_logic;
        i_rst   : in std_logic;
        i_start : in std_logic;
        i_done : in std_logic;
        i_k : in std_logic_vector(9 downto 0);
        i_count: in std_logic_vector(10 downto 0);
        
        o_mem_en : out std_logic;
        o_mem_we : out std_logic;
        o_en_done: out std_logic;
        o_en_new_read: out std_logic;
        o_en_data_handler : out std_logic;
        o_en_addr_handler: out std_logic;
        o_en_out_handler: out std_logic;
        o_sel_out_handler: out std_logic
    );
end component fsm;

signal en_preg_new_data: std_logic;
signal en_data_handler: std_logic;
signal en_addr_handler: std_logic;
signal en_out_handler: std_logic;
signal sel_out_handler: std_logic;
signal en_sreg_done: std_logic;

signal new_data: std_logic_vector (7 downto 0);
signal data: std_logic_vector(7 downto 0);
signal credibility: std_logic_vector(4 downto 0);
signal counter_value: std_logic_vector(10 downto 0);
signal done_value: std_logic;

begin

    preg_new_data : parallel_reg_8 port map(
        input => i_mem_data,
        output => new_data,
        enable => en_preg_new_data,
        i_clk => i_clk,
        i_rst => i_rst,
        i_done => done_value
    );
    
    addr_handler_0 : address_handler port map(
        i_en => en_addr_handler,
        i_clk => i_clk,
        i_rst => i_rst,
        i_add => i_add,
        i_done => done_value,
        
        o_addr => o_mem_addr,
        o_count => counter_value
    );
    
    data_handler_0 : data_handler port map(
        i_en => en_data_handler,
        i_clk => i_clk,
        i_rst => i_rst,
        i_new_data => new_data,
        i_done => done_value,
        
       
        o_data => data,
        o_credibility => credibility
    );
    
    out_handler_0 : output_handler port map(
        i_en => en_out_handler,
        i_sel => sel_out_handler,
        i_count => counter_value,
        i_k => i_k,
        i_data => data,
        i_credibility => credibility,
        
        o_output => o_mem_data
    );
    
    sreg_done : shift_reg_1 port map(
        enable => en_sreg_done,
        output => o_done,
        value => done_value,
        i_start => i_start,
        i_clk => i_clk,
        i_rst => i_rst
    );
    
    fsm_0 : fsm port map(
        i_clk => i_clk,
        i_rst => i_rst,
        i_start => i_start,
        i_done => done_value,
        i_k => i_k,
        i_count => counter_value,
        
        o_mem_en => o_mem_en,
        o_mem_we => o_mem_we,
        o_en_done => en_sreg_done,
        o_en_new_read => en_preg_new_data,
        o_en_data_handler => en_data_handler,
        o_en_addr_handler => en_addr_handler,
        o_en_out_handler => en_out_handler,
        o_sel_out_handler => sel_out_handler
    );

end project_reti_logiche_arch;


----------------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity parallel_reg_8 is
    Port ( 
        input : in std_logic_vector (7 downto 0);
        output : out std_logic_vector (7 downto 0);
        enable : in std_logic;
        i_clk : in std_logic;
        i_rst : in std_logic;
        i_done : in std_logic
    );
end parallel_reg_8;

architecture parallel_reg_8_arch of parallel_reg_8 is
signal stored_value : std_logic_vector (7 downto 0);
begin
    output <= stored_value;
    
    process(i_clk, i_rst)
    begin
        if i_rst = '1' then
            stored_value <= (others => '0');
        elsif i_clk'event and i_clk = '1' then
            if i_done = '1' then
                stored_value <= (others => '0');
            elsif i_done = '0' and enable = '1' then
                stored_value <= input;
            end if;
        end if;
    end process;
    
end parallel_reg_8_arch;


----------------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity shift_reg_1 is
    Port ( 
        enable: in std_logic;
        output : out std_logic;
        value : out std_logic;
        i_start : in std_logic;
        i_clk : in std_logic;
        i_rst : in std_logic
    );
end shift_reg_1;

architecture shift_reg_1_arch of shift_reg_1 is
signal stored_value : std_logic;
begin
    output <= stored_value;
    value <= stored_value;
    
    process(i_clk, i_rst)
    begin
        if i_rst = '1' then
            stored_value <= '0';
        elsif i_clk'event and i_clk = '1' then
            if i_start = '0' then 
                 stored_value <= '0';
            else
                if enable = '1' then
                    stored_value <= '1';
                end if;    
            end if;     
        end if;
    end process;

end shift_reg_1_arch;


----------------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity address_handler is
    Port ( 
        i_en : in std_logic;
        i_clk : in std_logic;
        i_rst : in std_logic;
        i_add:  in std_logic_vector(15 downto 0);
        i_done: in std_logic;
        o_addr : out std_logic_vector(15 downto 0);
        o_count : out std_logic_vector(10 downto 0)
    );
end address_handler;

architecture address_handler_arch of address_handler is

signal stored_counter : std_logic_vector (10 downto 0);
begin

    o_count <= stored_counter;
    o_addr <= std_logic_vector(unsigned(i_add)+ unsigned(stored_counter));
    
    process(i_clk, i_rst)
    begin
        if i_rst = '1' then
            stored_counter <= (others => '0');
        elsif i_clk'event and i_clk = '1' then
            if i_done = '1' then
                stored_counter <= (others => '0');
            else            
                if i_en  = '1' then
                    stored_counter <= std_logic_vector( unsigned(stored_counter) + 1 );   
                end if;
            end if;    
        end if;
    end process;


end address_handler_arch;


----------------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity data_handler is
    Port ( 
        i_en : in std_logic;
        i_clk : in std_logic;
        i_rst : in std_logic;
        i_new_data: in std_logic_vector(7 downto 0);
        i_done: in std_logic;
        
        o_data : out std_logic_vector(7 downto 0);
        o_credibility : out std_logic_vector(4 downto 0) 
    );
end data_handler;

architecture data_handler_arch of data_handler is

signal stored_last_pos_data : std_logic_vector(7 downto 0);
signal stored_credibility : std_logic_vector(4 downto 0);
begin
    o_credibility <= stored_credibility;
    
    process(i_clk, i_rst)
    begin
        
        if i_rst = '1' then
            stored_last_pos_data <= (others => '0');
            stored_credibility <= (others => '0');
            o_data <= (others => '0');
        elsif i_clk'event and i_clk = '1' then
            if i_done = '1' then 
                stored_last_pos_data <= (others => '0');
                stored_credibility <= (others => '0');
                o_data <= (others => '0');
            elsif  i_done = '0' and i_en  = '1' then
                if unsigned(i_new_data) /= 0 then
                    o_data <= i_new_data;
                    stored_last_pos_data <= i_new_data;
                    stored_credibility <= "11111";
                elsif unsigned(i_new_data) = 0 then
                    o_data <= stored_last_pos_data;
                    if unsigned(stored_credibility) /= 0 then
                        stored_credibility <= std_logic_vector( unsigned(stored_credibility) - 1 );
                    end if;  
                end if;   
            end if;
        end if;
    end process;

end data_handler_arch;


----------------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity output_handler is
    Port (
        i_en : in std_logic;
        i_sel : in std_logic;
        i_count : in std_logic_vector(10 downto 0);
        i_k : in std_logic_vector(9 downto 0);
        i_data: in std_logic_vector(7 downto 0);
        i_credibility: in std_logic_vector(4 downto 0);
        
        o_output: out std_logic_vector(7 downto 0)

    );
end output_handler;

architecture output_handler_arch of output_handler is

begin
    
    
    process(i_en, i_sel)
    begin
    
    if i_en = '1' then
        case i_sel is
            when '0' => o_output <= i_data;
            when '1' => o_output <= ("000" & i_credibility);
            when others => 
        end case;
      
    else
        o_output <=(others => '0');    
    end if;

    end process;
end output_handler_arch;


----------------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity fsm is
    Port ( 
        i_clk   : in std_logic;
        i_rst   : in std_logic;
        i_start : in std_logic;
        i_done : in std_logic;
        i_k : in std_logic_vector(9 downto 0);
        i_count : in std_logic_vector(10 downto 0);
        
        o_mem_en : out std_logic;
        o_mem_we : out std_logic;
        o_en_done: out std_logic;
        o_en_new_read: out std_logic;
        o_en_data_handler : out std_logic;
        o_en_addr_handler: out std_logic;
        o_en_out_handler: out std_logic;
        o_sel_out_handler: out std_logic

    );
end fsm;

architecture fsm_arch of fsm is
    type S is (
        INITIAL, 
        READ_PREPARE, 
        READ_MEM, 
        HANDLE_DATA,
        WRITE_MEM_D,
        WRITE_MEM_C
    );
    signal curr_state : S;
begin
    process(i_clk, i_rst)
    begin
        if i_rst = '1' then
            curr_state <= INITIAL;
        elsif i_clk'event and i_clk = '1' then
            case curr_state is
                when INITIAL =>
                    if i_start = '1' and i_done = '0' then
                        curr_state <= READ_PREPARE;
                    end if;
                when READ_PREPARE => 
                    if i_done = '1' or unsigned(i_k) = 0 then
                        curr_state <= INITIAL;
                    else 
                        curr_state <= READ_MEM;
                    end if;      
                when READ_MEM =>
                    curr_state <= HANDLE_DATA;
                when HANDLE_DATA =>
                    curr_state <= WRITE_MEM_D;
                when WRITE_MEM_D =>
                    curr_state <= WRITE_MEM_C;
                when WRITE_MEM_C =>
                    if i_done = '1' then
                        curr_state <= INITIAL;
                    else 
                        curr_state <= READ_PREPARE;
                    end if;   
            end case;
        end if;
    end process;
    
    process(curr_state)
    begin
        o_mem_en <= '0';
        o_mem_we <= '0';
        o_en_new_read <= '0';
        o_en_data_handler <= '0';
        o_en_addr_handler <= '0';
        o_en_out_handler <= '0';
        o_sel_out_handler <= '0';
        o_en_done <= '0';

        if curr_state = INITIAL then
    
        elsif curr_state = READ_PREPARE then
            if not(unsigned(i_k) > 0) then
                o_en_done <= '1';
            else
                o_mem_en <= '1';
                o_mem_we <= '0';     
            end if;  
        elsif curr_state = READ_MEM then
            o_en_new_read <= '1';
        elsif curr_state = HANDLE_DATA then
            o_en_data_handler <= '1';
        elsif curr_state = WRITE_MEM_D then
            o_mem_en <= '1';
            o_mem_we <= '1';
            o_en_out_handler <= '1';
            o_sel_out_handler <= '0';
            o_en_addr_handler <= '1';
        elsif curr_state = WRITE_MEM_C then
            o_mem_en <= '1';
            o_mem_we <= '1';
            o_en_out_handler <= '1';
            o_sel_out_handler <= '1';
            o_en_addr_handler <= '1'; 
            if unsigned(i_count) >= (unsigned(i_k)*2 -1) then 
                o_en_done <= '1';  
            end if;    
        end if;
    end process;

end fsm_arch;
