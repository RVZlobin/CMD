library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity queue is --очередь команд (кольцевой буфер с последующим сериализатором)
	generic (
		DBIT				: INTEGER := 8; --посылка 
		ID_DATA			: INTEGER := 1; --идентификатор устройства (длина в DBIT)
		CMD_ID_DATA		: INTEGER := 2; --идентификатор команды (длина в DBIT)
		CMD_DATA			: INTEGER := 1; --команда (длина в DBIT)
		LEN_DATA			: INTEGER := 1; --длинна данных (длина в DBIT)
		IND_DATA			: INTEGER := 1; --индекс (длина в DBIT)
		SUB_IND_DATA	: INTEGER := 2; --под индекс (длина в DBIT)
		MAX_DATA			: INTEGER := 8; --максимально возможная длинна данных (длина в DBIT)
		SUM_DATA			: INTEGER := 2; --контрольная сумма (инвертированные последние байты) (длина в DBIT)
		LEN_QUEUE		: INTEGER := 10 --длинна очереди
	);
	port (
		clk					: in STD_LOGIC := '0'; --тактовая частота
		reset					: in STD_LOGIC := '0'; --сброс асинхронный
		queue_put			: in STD_LOGIC := '0'; --входные данные сформированы
		tx_empty				: in STD_LOGIC := '0'; --передатчик свободен 
		get_next				: in STD_LOGIC := '0'; --запрос следующей 
		dev_id				: in STD_LOGIC_VECTOR(ID_DATA * DBIT - 1 downto 0)      := (others => 'X'); --идентификатор устройства вход
		cmd_id				: in STD_LOGIC_VECTOR(CMD_ID_DATA * DBIT - 1 downto 0)  := (others => 'X'); --идентификатор команды
		cmd					: in STD_LOGIC_VECTOR(CMD_DATA * DBIT - 1 downto 0)     := (others => 'X'); --команда
		data_len				: in STD_LOGIC_VECTOR(LEN_DATA * DBIT - 1 downto 0)     := (others => 'X'); --длинна данных
		index					: in STD_LOGIC_VECTOR(IND_DATA * DBIT - 1 downto 0)     := (others => 'X'); --индекс
		sub_index			: in STD_LOGIC_VECTOR(SUB_IND_DATA * DBIT - 1 downto 0) := (others => 'X'); --подиндекс
		data					: in STD_LOGIC_VECTOR(MAX_DATA * DBIT - 1 downto 0)     := (others => 'X'); --данные
		data_serial			: out STD_LOGIC_VECTOR(DBIT - 1 downto 0)    := (others => 'X'); --сериализованные данные
		data_serial_fix	: out STD_LOGIC := '0'; -- флаг установки сериализованых данных
		queue_fix	      : out STD_LOGIC := '0'; -- выходные данные сформированы
		clear_flag		  	: out STD_LOGIC := '1'; -- флаг нет данных
		replete_flag	  	: out STD_LOGIC := '0'  -- флаг переполнение
	);
	
	type SEND_RECORD_TYPE is record
		rec_dev_id			: STD_LOGIC_VECTOR(ID_DATA * DBIT - 1 downto 0);
		rec_cmd_id			: STD_LOGIC_VECTOR(CMD_ID_DATA * DBIT - 1 downto 0);
		rec_cmd				: STD_LOGIC_VECTOR(CMD_DATA * DBIT - 1 downto 0);
		rec_data_len		: STD_LOGIC_VECTOR(LEN_DATA * DBIT - 1 downto 0);
		rec_index			: STD_LOGIC_VECTOR(IND_DATA * DBIT - 1 downto 0);
		rec_sub_index		: STD_LOGIC_VECTOR(SUB_IND_DATA * DBIT - 1 downto 0);
		rec_data				: STD_LOGIC_VECTOR(MAX_DATA * DBIT - 1 downto 0);
	end record SEND_RECORD_TYPE;  
 
	constant SEND_RECORD_INIT : SEND_RECORD_TYPE := (
		rec_dev_id			=> (others => '0'),
		rec_cmd_id			=> (others => '0'),
		rec_cmd				=> (others => '0'),
		rec_data_len		=> (others => '0'),
		rec_index			=> (others => '0'),
		rec_sub_index		=> (others => '0'),
		rec_data				=> (others => '0')
	);
	
	type MEM_SEND_RECORD_TYPE is array (0 to LEN_QUEUE - 1) of SEND_RECORD_TYPE;
	subtype SERIAL_QUANTUM is STD_LOGIC_VECTOR(DBIT - 1 downto 0);
	type SERIAL_DATA_TYPE is array (0 to ID_DATA + CMD_ID_DATA + CMD_DATA + LEN_DATA + IND_DATA + SUB_IND_DATA + MAX_DATA + SUM_DATA - 1) of SERIAL_QUANTUM;
	 
	type STP_QUEUE_TYPE is (init, serializ_record, send, complit_send);
	type STP_SEND_TYPE is (data_set, send_wait, complit);
	
end queue;	

architecture queue_impl of queue is
	-- Параллельная дешифрация младший разряд первый 
	shared variable serial_data: SERIAL_DATA_TYPE;
	shared variable queue_mem	: MEM_SEND_RECORD_TYPE;

	shared variable index_queue_begin : INTEGER range 0 to LEN_QUEUE - 1 := 0;
	shared variable index_queue_end   : INTEGER range 0 to LEN_QUEUE - 1 := 0;
	shared variable queue_clear       : STD_LOGIC := '1';
	shared variable queue_replete     : STD_LOGIC := '0';
	shared variable state_queue	    : STP_QUEUE_TYPE  := init;
	shared variable stp_send          : STP_SEND_TYPE   := data_set;

	signal queue_clear_set        : STD_LOGIC := '0';
	signal queue_replete_dropping : STD_LOGIC := '0';
	signal data_serial_reg        : STD_LOGIC_VECTOR(DBIT - 1 downto 0)  := (others => 'X');
	signal data_serial_next       : STD_LOGIC_VECTOR(DBIT - 1 downto 0)  := (others => 'X');
	signal data_serial_fix_reg    : STD_LOGIC := '0';
	signal data_serial_fix_next   : STD_LOGIC := '0';

	signal queue_in_reg				: SEND_RECORD_TYPE := SEND_RECORD_INIT;
	signal clear_flag_reg			: STD_LOGIC := '1';
	signal replete_flag_reg			: STD_LOGIC := '1';

	signal dev_id_reg		: STD_LOGIC_VECTOR(ID_DATA * DBIT - 1 downto 0)		:= (others => '0');
	
begin

	data_serial     <= data_serial_reg;
	data_serial_fix <= data_serial_fix_reg;
	clear_flag      <= clear_flag_reg;
	replete_flag    <= replete_flag_reg;

	queue_reg_worck: process (clk, reset)
	begin
		if(reset = '1') then
			data_serial_fix_reg	<= '0';
			replete_flag_reg		<= '0';
			clear_flag_reg			<= '1';
		elsif(rising_edge(clk)) then
			data_serial_reg		<= data_serial_next;
			data_serial_fix_reg	<= data_serial_fix_next;
			clear_flag_reg			<= queue_clear;
			replete_flag_reg		<= queue_replete;
		end if;
	end process;
  
	queue_put_worck: process (queue_put, queue_replete_dropping, queue_clear_set, reset)
	begin
		if(reset = '1') then
			index_queue_begin := 0;
			queue_replete := '0';
		elsif(queue_replete_dropping = '1') then
			queue_replete := '0';
		elsif(queue_clear_set = '1') then
			queue_clear := '1';
		elsif(queue_put'event and queue_put = '1') then
			if(queue_replete = '0') then
				queue_mem(index_queue_begin) := (
					rec_dev_id			=> dev_id,
					rec_cmd_id			=> cmd_id,
					rec_cmd				=> cmd,
					rec_data_len		=> data_len,
					rec_index			=> index,
					rec_sub_index		=> sub_index,
					rec_data				=> data
				);
				if(index_queue_begin = LEN_QUEUE - 1) then
					index_queue_begin := 0;
				else
					index_queue_begin := index_queue_begin + 1;
				end if;
				if(index_queue_begin = index_queue_end) then
					queue_replete := '1';
				end if;
				queue_clear := '0';
			end if;
		end if;
	end process;
	  
	queue_send_worck: process (clk, reset)
		variable check_sum			: STD_LOGIC_VECTOR(SUM_DATA * DBIT - 1 downto 0)	:= (others => '0');
		variable serialize_index	: INTEGER RANGE 0 to (ID_DATA + CMD_ID_DATA + CMD_DATA + LEN_DATA + IND_DATA + SUB_IND_DATA + MAX_DATA + SUM_DATA) := 0; 
		variable send_index			: INTEGER RANGE 0 to (ID_DATA + CMD_ID_DATA + CMD_DATA + LEN_DATA + IND_DATA + SUB_IND_DATA + MAX_DATA + SUM_DATA) := 0; 
		variable temp_record			: SEND_RECORD_TYPE := SEND_RECORD_INIT;
	begin
		if(reset = '1') then
			state_queue           := init;
			stp_send              := data_set;
			send_index            := 0;
			serialize_index       := 0;
			data_serial_fix_next  <= '0';
			temp_record           := SEND_RECORD_INIT;
		elsif(rising_edge(clk)) then
			queue_clear_set         <= '0';
			queue_replete_dropping  <= '0';
			data_serial_fix_next    <= '0';
			case state_queue is
				when init =>
					if(queue_clear = '0') then
						temp_record := queue_mem(index_queue_end);
						if(index_queue_end = LEN_QUEUE - 1) then
						  index_queue_end := 0;
						else
						  index_queue_end := index_queue_end + 1;
						end if;
						if(queue_replete = '1') then
						  queue_replete_dropping <= '1';
						end if;
						if(index_queue_begin = index_queue_end) then
						  queue_clear_set <= '1';
						end if;
						state_queue := serializ_record;
					end if;
				when serializ_record =>
					check_sum				:= (others => '0');
					serialize_index := 0;
					for item in 0 to ID_DATA - 1 loop 
						serial_data(item + serialize_index) := temp_record.rec_dev_id(item * DBIT + DBIT - 1 downto item * DBIT);
						check_sum := STD_LOGIC_VECTOR(UNSIGNED(check_sum) + resize(UNSIGNED(temp_record.rec_dev_id(item * DBIT + DBIT - 1 downto item * DBIT)), check_sum'length));
					end loop;
					serialize_index := serialize_index + ID_DATA;
					for item in 0 to CMD_ID_DATA - 1 loop 
						serial_data(item + serialize_index) := temp_record.rec_cmd_id(item * DBIT + DBIT - 1 downto item * DBIT);
						check_sum := STD_LOGIC_VECTOR(UNSIGNED(check_sum) + resize(UNSIGNED(temp_record.rec_cmd_id(item * DBIT + DBIT - 1 downto item * DBIT)), check_sum'length));
					end loop;
					serialize_index := serialize_index + CMD_ID_DATA;
					for item in 0 to CMD_DATA - 1 loop 
						serial_data(item + serialize_index) := temp_record.rec_cmd(item * DBIT + DBIT - 1 downto item * DBIT);
						check_sum := STD_LOGIC_VECTOR(UNSIGNED(check_sum) + resize(UNSIGNED(temp_record.rec_cmd(item * DBIT + DBIT - 1 downto item * DBIT)), check_sum'length));
					end loop;
					serialize_index := serialize_index + CMD_DATA;
					for item in 0 to LEN_DATA - 1 loop 
						serial_data(item + serialize_index) := temp_record.rec_data_len(item * DBIT + DBIT - 1 downto item * DBIT);
						check_sum := STD_LOGIC_VECTOR(UNSIGNED(check_sum) + resize(UNSIGNED(temp_record.rec_data_len(item * DBIT + DBIT - 1 downto item * DBIT)), check_sum'length));
					end loop;
					serialize_index := serialize_index + LEN_DATA;
					for item in 0 to IND_DATA - 1 loop 
						serial_data(item + serialize_index) := temp_record.rec_index(item * DBIT + DBIT - 1 downto item * DBIT);
						check_sum := STD_LOGIC_VECTOR(UNSIGNED(check_sum) + resize(UNSIGNED(temp_record.rec_index(item * DBIT + DBIT - 1 downto item * DBIT)), check_sum'length));
					end loop;
					serialize_index := serialize_index + IND_DATA;
					for item in 0 to SUB_IND_DATA - 1 loop 
						serial_data(item + serialize_index) := temp_record.rec_sub_index(item * DBIT + DBIT - 1 downto item * DBIT);
						check_sum := STD_LOGIC_VECTOR(UNSIGNED(check_sum) + resize(UNSIGNED(temp_record.rec_sub_index(item * DBIT + DBIT - 1 downto item * DBIT)), check_sum'length));
					end loop;
					serialize_index := serialize_index + SUB_IND_DATA;
					for item in 0 to MAX_DATA - 1 loop 
						serial_data(item + serialize_index) := temp_record.rec_data(item * DBIT + DBIT - 1 downto item * DBIT);
						check_sum := STD_LOGIC_VECTOR(UNSIGNED(check_sum) + resize(UNSIGNED(temp_record.rec_data(item * DBIT + DBIT - 1 downto item * DBIT)), check_sum'length));
					end loop;
					--FIX вероятно неоднозначное поведение (нужные данные подтягивать во время отправки)
					serialize_index := serialize_index + TO_INTEGER(UNSIGNED(temp_record.rec_data_len));
					for item in 0 to SUM_DATA - 1 loop 
						serial_data(item + serialize_index) := not check_sum(item * DBIT + DBIT - 1 downto item * DBIT);
					end loop;
					serialize_index := serialize_index + SUM_DATA;
					state_queue := send;
				when send =>
					case stp_send is
						when data_set =>
							if(tx_empty = '1') then
								 data_serial_fix_next  <= '1';
								 data_serial_next      <= serial_data(send_index);
								 send_index            := send_index + 1;
								 stp_send              := send_wait;
							end if;
						when send_wait =>
							if(tx_empty = '0') then
								stp_send              := complit;
							end if;
						when complit =>
							stp_send  := data_set;
							if(send_index = serialize_index) then
								state_queue := complit_send;
								send_index := 0;
							end if;
					end case;
				when complit_send =>
					temp_record := SEND_RECORD_INIT;
					state_queue := init;
			end case;
		end if;
	end process;
end queue_impl;