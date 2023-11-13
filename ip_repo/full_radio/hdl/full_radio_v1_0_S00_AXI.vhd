library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity full_radio_v1_0_S00_AXI is
	generic (
		-- Users to add parameters here

		-- User parameters ends
		-- Do not modify the parameters beyond this line

		-- Width of S_AXI data bus
		C_S_AXI_DATA_WIDTH	: integer	:= 32;
		-- Width of S_AXI address bus
		C_S_AXI_ADDR_WIDTH	: integer	:= 4
	);
	port (
		-- Users to add ports here
        m_axis_tdata : out std_logic_vector(31 downto 0);
        m_axis_tvalid : out std_logic;
		-- User ports ends
		-- Do not modify the ports beyond this line

		-- Global Clock Signal
		S_AXI_ACLK	: in std_logic;
		-- Global Reset Signal. This Signal is Active LOW
		S_AXI_ARESETN	: in std_logic;
		-- Write address (issued by master, acceped by Slave)
		S_AXI_AWADDR	: in std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
		-- Write channel Protection type. This signal indicates the
    		-- privilege and security level of the transaction, and whether
    		-- the transaction is a data access or an instruction access.
		S_AXI_AWPROT	: in std_logic_vector(2 downto 0);
		-- Write address valid. This signal indicates that the master signaling
    		-- valid write address and control information.
		S_AXI_AWVALID	: in std_logic;
		-- Write address ready. This signal indicates that the slave is ready
    		-- to accept an address and associated control signals.
		S_AXI_AWREADY	: out std_logic;
		-- Write data (issued by master, acceped by Slave) 
		S_AXI_WDATA	: in std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
		-- Write strobes. This signal indicates which byte lanes hold
    		-- valid data. There is one write strobe bit for each eight
    		-- bits of the write data bus.    
		S_AXI_WSTRB	: in std_logic_vector((C_S_AXI_DATA_WIDTH/8)-1 downto 0);
		-- Write valid. This signal indicates that valid write
    		-- data and strobes are available.
		S_AXI_WVALID	: in std_logic;
		-- Write ready. This signal indicates that the slave
    		-- can accept the write data.
		S_AXI_WREADY	: out std_logic;
		-- Write response. This signal indicates the status
    		-- of the write transaction.
		S_AXI_BRESP	: out std_logic_vector(1 downto 0);
		-- Write response valid. This signal indicates that the channel
    		-- is signaling a valid write response.
		S_AXI_BVALID	: out std_logic;
		-- Response ready. This signal indicates that the master
    		-- can accept a write response.
		S_AXI_BREADY	: in std_logic;
		-- Read address (issued by master, acceped by Slave)
		S_AXI_ARADDR	: in std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
		-- Protection type. This signal indicates the privilege
    		-- and security level of the transaction, and whether the
    		-- transaction is a data access or an instruction access.
		S_AXI_ARPROT	: in std_logic_vector(2 downto 0);
		-- Read address valid. This signal indicates that the channel
    		-- is signaling valid read address and control information.
		S_AXI_ARVALID	: in std_logic;
		-- Read address ready. This signal indicates that the slave is
    		-- ready to accept an address and associated control signals.
		S_AXI_ARREADY	: out std_logic;
		-- Read data (issued by slave)
		S_AXI_RDATA	: out std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
		-- Read response. This signal indicates the status of the
    		-- read transfer.
		S_AXI_RRESP	: out std_logic_vector(1 downto 0);
		-- Read valid. This signal indicates that the channel is
    		-- signaling the required read data.
		S_AXI_RVALID	: out std_logic;
		-- Read ready. This signal indicates that the master can
    		-- accept the read data and response information.
		S_AXI_RREADY	: in std_logic
	);
end full_radio_v1_0_S00_AXI;

architecture arch_imp of full_radio_v1_0_S00_AXI is

	-- AXI4LITE signals
	signal axi_awaddr	: std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
	signal axi_awready	: std_logic;
	signal axi_wready	: std_logic;
	signal axi_bresp	: std_logic_vector(1 downto 0);
	signal axi_bvalid	: std_logic;
	signal axi_araddr	: std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
	signal axi_arready	: std_logic;
	signal axi_rdata	: std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
	signal axi_rresp	: std_logic_vector(1 downto 0);
	signal axi_rvalid	: std_logic;

	-- Example-specific design signals
	-- local parameter for addressing 32 bit / 64 bit C_S_AXI_DATA_WIDTH
	-- ADDR_LSB is used for addressing 32/64 bit registers/memories
	-- ADDR_LSB = 2 for 32 bits (n downto 2)
	-- ADDR_LSB = 3 for 64 bits (n downto 3)
	constant ADDR_LSB  : integer := (C_S_AXI_DATA_WIDTH/32)+ 1;
	constant OPT_MEM_ADDR_BITS : integer := 1;
	------------------------------------------------
	---- Signals for user logic register space example
	--------------------------------------------------
	---- Number of Slave Registers 4
	signal slv_reg0	:std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
	signal slv_reg1	:std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
	signal slv_reg2	:std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
	signal slv_reg3	:std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
	signal slv_reg_rden	: std_logic;
	signal slv_reg_wren	: std_logic;
	signal reg_data_out	:std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
	signal byte_index	: integer;
	signal aw_en	: std_logic;
	
	-- Free running counter.
	signal free_counter : unsigned(31 downto 0);
	
	--Storing state of filter select.
    signal filter_select : std_logic;
	
	--Input to dac if, which will be constructed from dds output.
    signal dac_if_input : std_logic_vector(31 downto 0);
    
    -- Peripheral output valid signal.
    signal valid_sig : std_logic;
    
    -- DDS reset.
    signal dds_reset_n : std_logic;
	
	--Internal signals to interface with fake adc dds.
    signal m_axis_data_tvalid_fake : std_logic;
    signal m_axis_data_tdata_fake : std_logic_vector(15 downto 0);
    signal ex_m_axis_data_tdata_fake : std_logic_vector(31 downto 0);
    
    --Internal signals to interface with tuner dds.
    signal m_axis_data_tvalid_tune : std_logic;
    signal m_axis_data_tdata_tune : std_logic_vector(31 downto 0);
    
    -- Signal to interface with multiplier.
    signal mult_a_valid_in : STD_LOGIC;
    signal mult_a_data_in : STD_LOGIC_VECTOR(31 DOWNTO 0);
    signal mult_b_valid_in : STD_LOGIC;
    signal mult_b_data_in : STD_LOGIC_VECTOR(31 DOWNTO 0);
    signal mult_valid_out : STD_LOGIC;
    signal mult_data_out : STD_LOGIC_VECTOR(79 DOWNTO 0);
    signal mult_out_imag : STD_LOGIC_VECTOR(32 DOWNTO 0);
    signal div_mult_out_imag : STD_LOGIC_VECTOR(15 DOWNTO 0);
    signal mult_out_real : STD_LOGIC_VECTOR(32 DOWNTO 0);
    signal div_mult_out_real : STD_LOGIC_VECTOR(15 DOWNTO 0);
    
    --Signals that interface with imag FIR filter.
    signal imag_fir0_tvalid_in : std_logic;
    signal imag_fir0_tdata_in : std_logic_vector(15 downto 0);
    signal imag_fir0_tready_out : std_logic;
    signal imag_fir0_tvalid_out : std_logic;
    signal imag_fir0_tdata_out : std_logic_vector(31 downto 0);
    
    signal imag_fir0_tdata_out_shift : std_logic_vector(15 downto 0);
    
    signal imag_fir1_tvalid_in : std_logic;
    signal imag_fir1_tdata_in : std_logic_vector(15 downto 0);
    signal imag_fir1_tready_out : std_logic;
    signal imag_fir1_tvalid_out : std_logic;
    signal imag_fir1_tdata_out : std_logic_vector(31 downto 0);
    
    signal imag_fir1_tdata_out_shift : std_logic_vector(15 downto 0);
    
    --Signals that interface with real FIR filter.
    signal real_fir0_tvalid_in : std_logic;
    signal real_fir0_tdata_in : std_logic_vector(15 downto 0);
    signal real_fir0_tready_out : std_logic;
    signal real_fir0_tvalid_out : std_logic;
    signal real_fir0_tdata_out : std_logic_vector(31 downto 0);
    
    signal real_fir0_tdata_out_shift : std_logic_vector(15 downto 0);
    
    signal real_fir1_tvalid_in : std_logic;
    signal real_fir1_tdata_in : std_logic_vector(15 downto 0);
    signal real_fir1_tready_out : std_logic;
    signal real_fir1_tvalid_out : std_logic;
    signal real_fir1_tdata_out : std_logic_vector(31 downto 0);
    
    signal real_fir1_tdata_out_shift : std_logic_vector(15 downto 0);
    
    
    COMPONENT dds_compiler_0
    PORT (
        aclk : IN STD_LOGIC;
        aresetn : IN STD_LOGIC;
        s_axis_phase_tvalid : IN STD_LOGIC;
        s_axis_phase_tdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        m_axis_data_tvalid : OUT STD_LOGIC;
        m_axis_data_tdata : OUT STD_LOGIC_VECTOR(15 DOWNTO 0)
    );
    END COMPONENT;
    
    -- define the tuner dds component
    COMPONENT dds_compiler_1
        PORT (
            aclk : IN STD_LOGIC;
            aresetn : IN STD_LOGIC;
            s_axis_phase_tvalid : IN STD_LOGIC;
            s_axis_phase_tdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
            m_axis_data_tvalid : OUT STD_LOGIC;
            m_axis_data_tdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0) 
    );
    END COMPONENT;
    
    -- complex multiplier to multiply fake ADC with tuner
    COMPONENT cmpy_0
    PORT (
        aclk : IN STD_LOGIC;
        s_axis_a_tvalid : IN STD_LOGIC;
        s_axis_a_tdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        s_axis_b_tvalid : IN STD_LOGIC;
        s_axis_b_tdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        m_axis_dout_tvalid : OUT STD_LOGIC;
        m_axis_dout_tdata : OUT STD_LOGIC_VECTOR(79 DOWNTO 0) 
    );
    END COMPONENT;
    
    COMPONENT fir_compiler_0
      PORT (
        aclk : IN STD_LOGIC;
        s_axis_data_tvalid : IN STD_LOGIC;
        s_axis_data_tready : OUT STD_LOGIC;
        s_axis_data_tdata : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
        m_axis_data_tvalid : OUT STD_LOGIC;
        m_axis_data_tdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0) 
      );
    END COMPONENT;
    
    COMPONENT fir_compiler_1
      PORT (
        aclk : IN STD_LOGIC;
        s_axis_data_tvalid : IN STD_LOGIC;
        s_axis_data_tready : OUT STD_LOGIC;
        s_axis_data_tdata : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
        m_axis_data_tvalid : OUT STD_LOGIC;
        m_axis_data_tdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0) 
      );
    END COMPONENT;

begin
	-- I/O Connections assignments

	S_AXI_AWREADY	<= axi_awready;
	S_AXI_WREADY	<= axi_wready;
	S_AXI_BRESP	<= axi_bresp;
	S_AXI_BVALID	<= axi_bvalid;
	S_AXI_ARREADY	<= axi_arready;
	S_AXI_RDATA	<= axi_rdata;
	S_AXI_RRESP	<= axi_rresp;
	S_AXI_RVALID	<= axi_rvalid;
	-- Implement axi_awready generation
	-- axi_awready is asserted for one S_AXI_ACLK clock cycle when both
	-- S_AXI_AWVALID and S_AXI_WVALID are asserted. axi_awready is
	-- de-asserted when reset is low.

	process (S_AXI_ACLK)
	begin
	  if rising_edge(S_AXI_ACLK) then 
	    if S_AXI_ARESETN = '0' then
	      axi_awready <= '0';
	      aw_en <= '1';
	    else
	      if (axi_awready = '0' and S_AXI_AWVALID = '1' and S_AXI_WVALID = '1' and aw_en = '1') then
	        -- slave is ready to accept write address when
	        -- there is a valid write address and write data
	        -- on the write address and data bus. This design 
	        -- expects no outstanding transactions. 
	           axi_awready <= '1';
	           aw_en <= '0';
	        elsif (S_AXI_BREADY = '1' and axi_bvalid = '1') then
	           aw_en <= '1';
	           axi_awready <= '0';
	      else
	        axi_awready <= '0';
	      end if;
	    end if;
	  end if;
	end process;

	-- Implement axi_awaddr latching
	-- This process is used to latch the address when both 
	-- S_AXI_AWVALID and S_AXI_WVALID are valid. 

	process (S_AXI_ACLK)
	begin
	  if rising_edge(S_AXI_ACLK) then 
	    if S_AXI_ARESETN = '0' then
	      axi_awaddr <= (others => '0');
	    else
	      if (axi_awready = '0' and S_AXI_AWVALID = '1' and S_AXI_WVALID = '1' and aw_en = '1') then
	        -- Write Address latching
	        axi_awaddr <= S_AXI_AWADDR;
	      end if;
	    end if;
	  end if;                   
	end process; 

	-- Implement axi_wready generation
	-- axi_wready is asserted for one S_AXI_ACLK clock cycle when both
	-- S_AXI_AWVALID and S_AXI_WVALID are asserted. axi_wready is 
	-- de-asserted when reset is low. 

	process (S_AXI_ACLK)
	begin
	  if rising_edge(S_AXI_ACLK) then 
	    if S_AXI_ARESETN = '0' then
	      axi_wready <= '0';
	    else
	      if (axi_wready = '0' and S_AXI_WVALID = '1' and S_AXI_AWVALID = '1' and aw_en = '1') then
	          -- slave is ready to accept write data when 
	          -- there is a valid write address and write data
	          -- on the write address and data bus. This design 
	          -- expects no outstanding transactions.           
	          axi_wready <= '1';
	      else
	        axi_wready <= '0';
	      end if;
	    end if;
	  end if;
	end process; 

	-- Implement memory mapped register select and write logic generation
	-- The write data is accepted and written to memory mapped registers when
	-- axi_awready, S_AXI_WVALID, axi_wready and S_AXI_WVALID are asserted. Write strobes are used to
	-- select byte enables of slave registers while writing.
	-- These registers are cleared when reset (active low) is applied.
	-- Slave register write enable is asserted when valid address and data are available
	-- and the slave is ready to accept the write address and write data.
	slv_reg_wren <= axi_wready and S_AXI_WVALID and axi_awready and S_AXI_AWVALID ;

	process (S_AXI_ACLK)
	variable loc_addr :std_logic_vector(OPT_MEM_ADDR_BITS downto 0); 
	begin
	  if rising_edge(S_AXI_ACLK) then 
	    if S_AXI_ARESETN = '0' then
	      slv_reg0 <= (others => '0');
	      slv_reg1 <= (others => '0');
	      slv_reg2 <= (others => '0');
	      slv_reg3 <= (others => '0');
	    else
	      loc_addr := axi_awaddr(ADDR_LSB + OPT_MEM_ADDR_BITS downto ADDR_LSB);
	      if (slv_reg_wren = '1') then
	        case loc_addr is
	          when b"00" =>
	            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
	              if ( S_AXI_WSTRB(byte_index) = '1' ) then
	                -- Respective byte enables are asserted as per write strobes                   
	                -- slave registor 0
	                slv_reg0(byte_index*8+7 downto byte_index*8) <= S_AXI_WDATA(byte_index*8+7 downto byte_index*8);
	              end if;
	            end loop;
	          when b"01" =>
	            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
	              if ( S_AXI_WSTRB(byte_index) = '1' ) then
	                -- Respective byte enables are asserted as per write strobes                   
	                -- slave registor 1
	                slv_reg1(byte_index*8+7 downto byte_index*8) <= S_AXI_WDATA(byte_index*8+7 downto byte_index*8);
	              end if;
	            end loop;
	          when b"10" =>
	            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
	              if ( S_AXI_WSTRB(byte_index) = '1' ) then
	                -- Respective byte enables are asserted as per write strobes                   
	                -- slave registor 2
	                slv_reg2(byte_index*8+7 downto byte_index*8) <= S_AXI_WDATA(byte_index*8+7 downto byte_index*8);
	              end if;
	            end loop;
	          when b"11" =>
	            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
	              if ( S_AXI_WSTRB(byte_index) = '1' ) then
	                -- Respective byte enables are asserted as per write strobes                   
	                -- slave registor 3
	                slv_reg3(byte_index*8+7 downto byte_index*8) <= S_AXI_WDATA(byte_index*8+7 downto byte_index*8);
	              end if;
	            end loop;
	          when others =>
	            slv_reg0 <= slv_reg0;
	            slv_reg1 <= slv_reg1;
	            slv_reg2 <= slv_reg2;
	            slv_reg3 <= slv_reg3;
	        end case;
	      end if;
	    end if;
	  end if;                   
	end process; 

	-- Implement write response logic generation
	-- The write response and response valid signals are asserted by the slave 
	-- when axi_wready, S_AXI_WVALID, axi_wready and S_AXI_WVALID are asserted.  
	-- This marks the acceptance of address and indicates the status of 
	-- write transaction.

	process (S_AXI_ACLK)
	begin
	  if rising_edge(S_AXI_ACLK) then 
	    if S_AXI_ARESETN = '0' then
	      axi_bvalid  <= '0';
	      axi_bresp   <= "00"; --need to work more on the responses
	    else
	      if (axi_awready = '1' and S_AXI_AWVALID = '1' and axi_wready = '1' and S_AXI_WVALID = '1' and axi_bvalid = '0'  ) then
	        axi_bvalid <= '1';
	        axi_bresp  <= "00"; 
	      elsif (S_AXI_BREADY = '1' and axi_bvalid = '1') then   --check if bready is asserted while bvalid is high)
	        axi_bvalid <= '0';                                 -- (there is a possibility that bready is always asserted high)
	      end if;
	    end if;
	  end if;                   
	end process; 

	-- Implement axi_arready generation
	-- axi_arready is asserted for one S_AXI_ACLK clock cycle when
	-- S_AXI_ARVALID is asserted. axi_awready is 
	-- de-asserted when reset (active low) is asserted. 
	-- The read address is also latched when S_AXI_ARVALID is 
	-- asserted. axi_araddr is reset to zero on reset assertion.

	process (S_AXI_ACLK)
	begin
	  if rising_edge(S_AXI_ACLK) then 
	    if S_AXI_ARESETN = '0' then
	      axi_arready <= '0';
	      axi_araddr  <= (others => '1');
	    else
	      if (axi_arready = '0' and S_AXI_ARVALID = '1') then
	        -- indicates that the slave has acceped the valid read address
	        axi_arready <= '1';
	        -- Read Address latching 
	        axi_araddr  <= S_AXI_ARADDR;           
	      else
	        axi_arready <= '0';
	      end if;
	    end if;
	  end if;                   
	end process; 

	-- Implement axi_arvalid generation
	-- axi_rvalid is asserted for one S_AXI_ACLK clock cycle when both 
	-- S_AXI_ARVALID and axi_arready are asserted. The slave registers 
	-- data are available on the axi_rdata bus at this instance. The 
	-- assertion of axi_rvalid marks the validity of read data on the 
	-- bus and axi_rresp indicates the status of read transaction.axi_rvalid 
	-- is deasserted on reset (active low). axi_rresp and axi_rdata are 
	-- cleared to zero on reset (active low).  
	process (S_AXI_ACLK)
	begin
	  if rising_edge(S_AXI_ACLK) then
	    if S_AXI_ARESETN = '0' then
	      axi_rvalid <= '0';
	      axi_rresp  <= "00";
	    else
	      if (axi_arready = '1' and S_AXI_ARVALID = '1' and axi_rvalid = '0') then
	        -- Valid read data is available at the read data bus
	        axi_rvalid <= '1';
	        axi_rresp  <= "00"; -- 'OKAY' response
	      elsif (axi_rvalid = '1' and S_AXI_RREADY = '1') then
	        -- Read data is accepted by the master
	        axi_rvalid <= '0';
	      end if;            
	    end if;
	  end if;
	end process;

	-- Implement memory mapped register select and read logic generation
	-- Slave register read enable is asserted when valid address is available
	-- and the slave is ready to accept the read address.
	slv_reg_rden <= axi_arready and S_AXI_ARVALID and (not axi_rvalid) ;

	process (slv_reg0, slv_reg1, slv_reg2, slv_reg3, axi_araddr, S_AXI_ARESETN, slv_reg_rden)
	variable loc_addr :std_logic_vector(OPT_MEM_ADDR_BITS downto 0);
	begin
	    -- Address decoding for reading registers
	    loc_addr := axi_araddr(ADDR_LSB + OPT_MEM_ADDR_BITS downto ADDR_LSB);
	    case loc_addr is
	      when b"00" =>
	        reg_data_out <= slv_reg0;
	      when b"01" =>
	        reg_data_out <= slv_reg1;
	      when b"10" =>
	        reg_data_out <= slv_reg2;
	      when b"11" =>
	        reg_data_out <= std_logic_vector(free_counter);
	      when others =>
	        reg_data_out  <= (others => '0');
	    end case;
	end process; 

	-- Output register or memory read data
	process( S_AXI_ACLK ) is
	begin
	  if (rising_edge (S_AXI_ACLK)) then
	    if ( S_AXI_ARESETN = '0' ) then
	      axi_rdata  <= (others => '0');
	    else
	      if (slv_reg_rden = '1') then
	        -- When there is a valid read address (S_AXI_ARVALID) with 
	        -- acceptance of read address by the slave (axi_arready), 
	        -- output the read dada 
	        -- Read address mux
	          axi_rdata <= reg_data_out;     -- register read data
	      end if;   
	    end if;
	  end if;
	end process;

	-- Add user logic here
	
    process (S_AXI_ACLK, S_AXI_ARESETN)
	begin
	  if rising_edge(S_AXI_ACLK) then 
	    if S_AXI_ARESETN = '0' then
	      free_counter <= "00000000000000000000000000000000";
	    else
	      free_counter <= free_counter + 1;
	    end if;
	  end if;
	end process;
	
	-- Peripheral output assignments.
	m_axis_tdata <= dac_if_input;
	m_axis_tvalid <= valid_sig;
	
	--construct the 32 bit word input to the dac interface
    filter_select <= '1';
    dac_if_input <= (imag_fir1_tdata_out_shift & real_fir1_tdata_out_shift) when (filter_select = '0') else
                    (imag_fir1_tdata_out_shift & real_fir1_tdata_out_shift);
    valid_sig <= imag_fir1_tvalid_out and real_fir1_tvalid_out;
    
    -- define dds reset signal.
    dds_reset_n <= '1' when (slv_reg2(0) = '0') else
                   '0';

  fake_adc : dds_compiler_0
  PORT MAP (
    aclk => s_axi_aclk,
    aresetn => dds_reset_n,
    s_axis_phase_tvalid => '1',
    s_axis_phase_tdata => slv_reg0,
    m_axis_data_tvalid => m_axis_data_tvalid_fake,
    m_axis_data_tdata => m_axis_data_tdata_fake
  );
  
  -- Make the fake adc dds output into 32 bits.
  ex_m_axis_data_tdata_fake <= "0000000000000000" & m_axis_data_tdata_fake(15 downto 0);
  
  tuner_adc : dds_compiler_1
  PORT MAP (
    aclk => s_axi_aclk,
    aresetn => dds_reset_n,
    s_axis_phase_tvalid => '1',
    s_axis_phase_tdata => slv_reg1,
    m_axis_data_tvalid => m_axis_data_tvalid_tune,
    m_axis_data_tdata => m_axis_data_tdata_tune
  );
  
    -- Define the inputs to the complex multiplier.
    mult_a_valid_in <= m_axis_data_tvalid_fake;
    mult_a_data_in <= ex_m_axis_data_tdata_fake;
    mult_b_valid_in <= m_axis_data_tvalid_tune;
    mult_b_data_in <= m_axis_data_tdata_tune;
  
  tune_mult : cmpy_0
    PORT MAP (
        aclk => s_axi_aclk,
        s_axis_a_tvalid => mult_a_valid_in,
        s_axis_a_tdata => mult_a_data_in,
        s_axis_b_tvalid => mult_b_valid_in,
        s_axis_b_tdata => mult_b_data_in,
        m_axis_dout_tvalid => mult_valid_out,
        m_axis_dout_tdata => mult_data_out
    );
    
     -- Divide up the multiplier output into imaginary and real.
     -- Scale down the multiplier outputs.
     mult_out_imag <= mult_data_out(72 downto 40);
     div_mult_out_imag <= mult_out_imag(32) & mult_out_imag(28 downto 14);
     mult_out_real <= mult_data_out(32 downto 0);
     div_mult_out_real <= mult_out_real(32) & mult_out_real(28 downto 14);
     
         
    imag_fir0_tvalid_in <= mult_valid_out;
    imag_fir0_tdata_in <= div_mult_out_imag;
    
    imag_fir0 : fir_compiler_0
      PORT MAP (
        aclk => s_axi_aclk,
        s_axis_data_tvalid => imag_fir0_tvalid_in,
        s_axis_data_tready => imag_fir0_tready_out,
        s_axis_data_tdata => imag_fir0_tdata_in,
        m_axis_data_tvalid => imag_fir0_tvalid_out,
        m_axis_data_tdata => imag_fir0_tdata_out
    );
    
    imag_fir0_tdata_out_shift <= imag_fir0_tdata_out(31) & imag_fir0_tdata_out(29 downto 15);
    
    imag_fir1_tvalid_in <= imag_fir0_tvalid_out;
    imag_fir1_tdata_in <= imag_fir0_tdata_out_shift;
    
    imag_fir1 : fir_compiler_1
      PORT MAP (
        aclk => s_axi_aclk,
        s_axis_data_tvalid => imag_fir1_tvalid_in,
        s_axis_data_tready => imag_fir1_tready_out,
        s_axis_data_tdata => imag_fir1_tdata_in,
        m_axis_data_tvalid => imag_fir1_tvalid_out,
        m_axis_data_tdata => imag_fir1_tdata_out
    );
    
    imag_fir1_tdata_out_shift <= imag_fir1_tdata_out(31) & imag_fir1_tdata_out(29 downto 15);
    
    real_fir0_tvalid_in <= mult_valid_out;
    real_fir0_tdata_in <= div_mult_out_real;
    
    real_fir0 : fir_compiler_0
      PORT MAP (
        aclk => s_axi_aclk,
        s_axis_data_tvalid => real_fir0_tvalid_in,
        s_axis_data_tready => real_fir0_tready_out,
        s_axis_data_tdata => real_fir0_tdata_in,
        m_axis_data_tvalid => real_fir0_tvalid_out,
        m_axis_data_tdata => real_fir0_tdata_out
    );
    
    real_fir0_tdata_out_shift <= real_fir0_tdata_out(31) & real_fir0_tdata_out(29 downto 15);
    
    real_fir1_tvalid_in <= real_fir0_tvalid_out;
    real_fir1_tdata_in <= real_fir0_tdata_out_shift;
    
    real_fir1 : fir_compiler_1
      PORT MAP (
        aclk => s_axi_aclk,
        s_axis_data_tvalid => real_fir1_tvalid_in,
        s_axis_data_tready => real_fir1_tready_out,
        s_axis_data_tdata => real_fir1_tdata_in,
        m_axis_data_tvalid => real_fir1_tvalid_out,
        m_axis_data_tdata => real_fir1_tdata_out
    );
    
    real_fir1_tdata_out_shift <= real_fir1_tdata_out(31) & real_fir1_tdata_out(29 downto 15);
    
	-- User logic ends

end arch_imp;
