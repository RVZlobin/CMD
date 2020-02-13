library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Request is
	generic (
		DBIT				: INTEGER := 8; --посылка 
		ID_DATA			: INTEGER := 1; --идентификатор устройства (длина в DBIT)
		CMD_ID_DATA		: INTEGER := 2; --идентификатор команды (длина в DBIT)
		CMD_DATA			: INTEGER := 1; --команда (длина в DBIT)
		LEN_DATA			: INTEGER := 1; --длинна данных (длина в DBIT)
		IND_DATA			: INTEGER := 1; --индекс (длина в DBIT)
		SUB_IND_DATA	: INTEGER := 2; --под индекс (длина в DBIT)
		MAX_DATA			: INTEGER := 8; --максимально возможная длинна данных (длина в DBIT)
		SUM_DATA			: INTEGER := 2 --; --контрольная сумма (инвертированные последние байты) (длина в DBIT)
	);
	port (
		clk					: in STD_LOGIC := '0'; --тактовая частота
		reset				   : in STD_LOGIC := '0'; --сброс асинхронный
		transmit			   : in STD_LOGIC := '1'; --посылка готова
		clr_flag			   : in STD_LOGIC := '0'; --сброс флага готовности команды (когда команда выполнена)
		data_in				: in STD_LOGIC_VECTOR(DBIT - 1 downto 0) := (others => 'X'); --данные посылки
		dev_id				: in STD_LOGIC_VECTOR(ID_DATA * DBIT - 1 downto 0) := (others => 'X'); --идентификатор устройства
		cmd_id				: out STD_LOGIC_VECTOR(CMD_ID_DATA * DBIT - 1 downto 0) := (others => 'X'); --идентификатор команды
		cmd					: out STD_LOGIC_VECTOR(CMD_DATA * DBIT - 1 downto 0) := (others => 'X'); --команда на выход
		index				   : out STD_LOGIC_VECTOR(IND_DATA * DBIT - 1 downto 0) := (others => 'X'); --индекс
		sub_index			: out STD_LOGIC_VECTOR(SUB_IND_DATA * DBIT - 1 downto 0) := (others => 'X');--подиндекс
		data_len			   : out STD_LOGIC_VECTOR(LEN_DATA * DBIT - 1 downto 0) := (others => 'X'); --длинна данных
		data_out			   : out STD_LOGIC_VECTOR(MAX_DATA * DBIT - 1 downto 0) := (others => 'X');--данные
		transmit_clr_flag	: out STD_LOGIC := '0'; --сигнал завершения обработки кадра 
		flag					: out STD_LOGIC := '0'; -- флаг готовности команды ля исполнения
		err_check_sum		: out STD_LOGIC := '0' -- флаг ошибки проверки контрольной сумы
	);
	type stp_data_type is (init, data, pars, err, complit); --стадии конечного автомата компоновки команды
	
	constant t_incr				: INTEGER := 1;
	constant ID_DATA_POS			: INTEGER := ID_DATA - 1;
	constant CMD_ID_DATA_POS	: INTEGER := (ID_DATA + CMD_ID_DATA) - 1;
	constant CMD_DATA_POS		: INTEGER := (ID_DATA + CMD_ID_DATA + CMD_DATA) - 1;
	constant LEN_DATA_POS		: INTEGER := (ID_DATA + CMD_ID_DATA + CMD_DATA + LEN_DATA) - 1;
	constant IND_DATA_POS		: INTEGER := (ID_DATA + CMD_ID_DATA + CMD_DATA + LEN_DATA + IND_DATA) - 1;
	constant SUB_IND_DATA_POS	: INTEGER := (ID_DATA + CMD_ID_DATA + CMD_DATA + LEN_DATA + IND_DATA + SUB_IND_DATA) - 1;
end Request;

architecture cmd_request of Request is
	--прием и обработка данных посылки осуществляется асинхронно тактовому сигналу
	--состояние конечного автомата фиксируется по тактовому сигналу.
	shared variable transmit_count: INTEGER range 0 to (ID_DATA + CMD_DATA + LEN_DATA + LEN_DATA + IND_DATA + SUB_IND_DATA + MAX_DATA + SUM_DATA - 1) := 0;
	shared variable stp_data      : stp_data_type := init;
	--FIX исправить разрядность контрольной суммы
	shared variable check_sum			: STD_LOGIC_VECTOR(15 downto 0)		  := (others => '0');
	
	signal receive_timeout        : STD_LOGIC := '0';
	signal dev_id_reg             : STD_LOGIC_VECTOR(ID_DATA * DBIT - 1 downto 0)			:= (others => '0');
	signal dev_id_reg_shift       : STD_LOGIC_VECTOR(ID_DATA * DBIT - 1 downto 0)			:= (others => '0');
	signal dev_id_next            : STD_LOGIC_VECTOR(ID_DATA * DBIT - 1 downto 0)			:= (others => '0');
	signal cmd_id_reg             : STD_LOGIC_VECTOR(CMD_ID_DATA * DBIT - 1 downto 0)	:= (others => '0');
	signal cmd_id_reg_shift       : STD_LOGIC_VECTOR(CMD_ID_DATA * DBIT - 1 downto 0)	:= (others => '0');
	signal cmd_id_next            : STD_LOGIC_VECTOR(CMD_ID_DATA * DBIT - 1 downto 0)	:= (others => '0');
	signal cmd_reg                : STD_LOGIC_VECTOR(CMD_DATA * DBIT - 1 downto 0)		:= (others => '0');
	signal cmd_reg_shift          : STD_LOGIC_VECTOR(CMD_DATA * DBIT - 1 downto 0)		:= (others => '0');
	signal cmd_next               : STD_LOGIC_VECTOR(CMD_DATA * DBIT - 1 downto 0)		:= (others => '0');
	signal data_len_reg           : STD_LOGIC_VECTOR(LEN_DATA * DBIT - 1 downto 0)		:= (others => '0');
	signal data_len_reg_shift     : STD_LOGIC_VECTOR(LEN_DATA * DBIT - 1 downto 0)		:= (others => '0');
	signal data_len_next          : STD_LOGIC_VECTOR(LEN_DATA * DBIT - 1 downto 0)		:= (others => '0');
	signal index_reg              : STD_LOGIC_VECTOR(IND_DATA * DBIT - 1 downto 0)		:= (others => '0');
	signal index_reg_shift        : STD_LOGIC_VECTOR(IND_DATA * DBIT - 1 downto 0)		:= (others => '0');
	signal index_next             : STD_LOGIC_VECTOR(IND_DATA * DBIT - 1 downto 0)		:= (others => '0');
	signal sub_index_reg          : STD_LOGIC_VECTOR(SUB_IND_DATA * DBIT - 1 downto 0)	:= (others => '0');
	signal sub_index_reg_shift    : STD_LOGIC_VECTOR(SUB_IND_DATA * DBIT - 1 downto 0)	:= (others => '0');
	signal sub_index_next         : STD_LOGIC_VECTOR(SUB_IND_DATA * DBIT - 1 downto 0)	:= (others => '0');
	signal data_in_reg            : STD_LOGIC_VECTOR(DBIT - 1 downto 0)						:= (others => 'X');
	signal data_out_reg           : STD_LOGIC_VECTOR(MAX_DATA * DBIT - 1 downto 0)		:= (others => '0');
	signal data_out_reg_shift     : STD_LOGIC_VECTOR(MAX_DATA * DBIT - 1 downto 0)		:= (others => '0');
	signal data_out_next          : STD_LOGIC_VECTOR(MAX_DATA * DBIT - 1 downto 0)		:= (others => '0');
	signal sum_reg                : STD_LOGIC_VECTOR(SUM_DATA * DBIT - 1 downto 0)		:= (others => '0');
	signal sum_reg_shift          : STD_LOGIC_VECTOR(SUM_DATA * DBIT - 1 downto 0)		:= (others => '0');
	signal sum_next               : STD_LOGIC_VECTOR(SUM_DATA * DBIT - 1 downto 0)		:= (others => '0');
	signal transmit_clr_flag_next : STD_LOGIC                                         	:= '0';
	signal transmit_clr_flag_reg  : STD_LOGIC                                         	:= '0';
	signal flag_reg               : STD_LOGIC                                         	:= '0';
	signal flag_next              : STD_LOGIC                                         	:= '0';
	signal err_check_sum_reg		: STD_LOGIC                                         	:= '0';
	signal err_check_sum_next		: STD_LOGIC                                         	:= '0';
	
begin
	
	transmit_clr_flag	<= '0' when reset = '1' else transmit_clr_flag_reg;
	
	process(clk, clr_flag, reset)
	begin
		if(reset = '1' or clr_flag = '1') then
			flag							<= '0';
			transmit_clr_flag_reg	<= '0';
			cmd							<= (others => '0');
			data_len						<= (others => '0');
			index				         <= (others => '0');
			sub_index			      <= (others => '0');
			data_out						<= (others => '0');
			dev_id_reg_shift 		  	<= (others => '0');
			cmd_id_reg_shift		  	<= (others => '0');
			cmd_reg_shift 			  	<= (others => '0');
			data_len_reg_shift 	  	<= (others => '0');
			index_reg_shift 		  	<= (others => '0');
			sub_index_reg_shift   	<= (others => '0');
			data_out_reg_shift 	  	<= (others => '0');
			sum_reg_shift         	<= (others => '0');
			flag_reg              	<= '0';
		elsif (rising_edge(clk)) then
			-- Процессы актуализации сдвиговых регистров
			transmit_clr_flag_reg	<= transmit_clr_flag_next;
			dev_id_reg_shift			<= dev_id_next;
			cmd_id_reg_shift			<= cmd_id_next;
			cmd_reg_shift				<= cmd_next;
			data_len_reg_shift		<= data_len_next;
			index_reg_shift			<= index_next;
			sub_index_reg_shift		<= sub_index_next;
			data_out_reg_shift		<= data_out_next;
			sum_reg_shift				<= sum_next;
			flag_reg						<= flag_next;
			err_check_sum_reg			<= err_check_sum_next;
			if(dev_id = dev_id_reg) then      -- Проверка предназначена ли принятая команда данному устройству
				cmd				<= cmd_reg;
				cmd_id			<= cmd_id_reg;
				data_len			<= data_len_reg;
				index				<= index_reg;
				sub_index		<= sub_index_reg;
				data_out			<= data_out_reg;
				flag				<= flag_reg;
				err_check_sum	<= err_check_sum_reg;
			end if;
		end if;
	end process;

	process(clk, clr_flag, reset)
    variable old_data: STD_LOGIC  := '0'; -- Признак не актуальных данных в регистре
	begin
		if(reset = '1' or clr_flag = '1') then
			stp_data						:= init;
			transmit_count				:= 0;
			check_sum					:= (others => '0');
			flag_next               <= '0';
			transmit_clr_flag_next	<= '0';
			dev_id_next					<= (others => '0');
			cmd_id_next					<= (others => '0');
			cmd_next						<= (others => '0');
			data_len_next				<= (others => '0');
			index_next					<= (others => '0');
			sub_index_next				<= (others => '0');
			data_out_next				<= (others => '0');
			sum_next						<= (others => '0');

			data_in_reg     <= (others => '0');
			dev_id_reg      <= (others => '0');
			cmd_id_reg      <= (others => '0');
			cmd_reg         <= (others => '0');
			data_len_reg    <= (others => '0');
			index_reg       <= (others => '0');
			sub_index_reg   <= (others => '0');
			data_out_reg    <= (others => '0');
		elsif(rising_edge(clk)) then
			case stp_data is
				when init =>
					-- Предварительная очистка сигналов
					flag_next		<= '0';
					dev_id_next		<= (others => '0');
					cmd_next			<= (others => '0');
					cmd_id_next		<= (others => '0');
					data_len_next	<= (others => '0');
					index_next		<= (others => '0');
					sub_index_next	<= (others => '0');
					data_out_next	<= (others => '0');
					sum_next			<= (others => '0');
					if(transmit = '0') then
						stp_data                := pars;					-- Переключение состаяния в режим парсера
						transmit_count          := 0;       			-- Сброс счетчика принятых кадров
						check_sum					:= (others => '0');	-- Сброс контрольной суммы
						data_in_reg             <= data_in;				-- Сохранение данных на входе в регистр
						transmit_clr_flag_next  <= '1';					-- Установка флага о принятии кадра
						err_check_sum_next		<= '0';					-- Сброс состояния ошибки
					end if;
				when data =>
					if(transmit = '0') then
						stp_data                := pars;    -- Переключение состаяния в режим парсера
						data_in_reg             <= data_in; -- Сохранение данных на входе в регистр
						transmit_clr_flag_next  <= '1';     -- Установка флага о принятии кадра
					end if;
				when pars =>
					transmit_clr_flag_next	<= '0';       -- Сброс флага о принятии кадра
					if(transmit = '1') then               -- Запуск парсера по возврату сигнала transmit в исходное состояние.
						stp_data  := data;                  -- Переключение состаяния в режим ожидания нового кадра
						if(transmit_count < ID_DATA_POS + 1) then
							--компоновка идентификатора устройства
							dev_id_next <= STD_LOGIC_VECTOR(SHIFT_LEFT(UNSIGNED(dev_id_reg_shift or STD_LOGIC_VECTOR(resize(UNSIGNED(data_in_reg), dev_id_reg_shift'length))), (ID_DATA_POS - transmit_count) * DBIT));
						elsif(transmit_count < CMD_DATA_POS + 1) then
							--компоновка идентификатора команды
							cmd_id_next <= STD_LOGIC_VECTOR(SHIFT_LEFT(UNSIGNED(cmd_id_reg_shift  or STD_LOGIC_VECTOR(resize(UNSIGNED(data_in_reg), cmd_id_reg_shift'length))), (CMD_ID_DATA_POS - transmit_count) * DBIT));	
						elsif(transmit_count < CMD_DATA_POS + 1) then
							--компоновка команды
							cmd_next <= STD_LOGIC_VECTOR(SHIFT_LEFT(UNSIGNED(cmd_reg_shift  or STD_LOGIC_VECTOR(resize(UNSIGNED(data_in_reg), cmd_reg_shift'length))), (CMD_DATA_POS - transmit_count) * DBIT));
						elsif(transmit_count < LEN_DATA_POS + 1) then
							--вычисление длинны сегмента с данными
							data_len_next <= STD_LOGIC_VECTOR(SHIFT_LEFT(UNSIGNED(data_len_reg_shift  or STD_LOGIC_VECTOR(resize(UNSIGNED(data_in_reg), data_len_reg_shift'length))), (LEN_DATA_POS - transmit_count) * DBIT));
						elsif(transmit_count < IND_DATA_POS + 1) then
							--компоновка индекса
							index_next <= STD_LOGIC_VECTOR(SHIFT_LEFT(UNSIGNED(index_reg_shift or STD_LOGIC_VECTOR(resize(UNSIGNED(data_in_reg), index_reg_shift'length))), (IND_DATA_POS - transmit_count) * DBIT));
						elsif(transmit_count < SUB_IND_DATA_POS + 1) then
						  --компоновка подиндекса
						  sub_index_next <= STD_LOGIC_VECTOR(SHIFT_LEFT(UNSIGNED(sub_index_reg_shift  or STD_LOGIC_VECTOR(resize(UNSIGNED(data_in_reg), sub_index_reg_shift'length))), (SUB_IND_DATA_POS - transmit_count) * DBIT));
						elsif(transmit_count < SUB_IND_DATA_POS + UNSIGNED(data_len_reg_shift) + 1) then
						  --компоновка данных
						  data_out_next <= STD_LOGIC_VECTOR(SHIFT_LEFT(UNSIGNED(data_out_reg_shift or STD_LOGIC_VECTOR(resize(UNSIGNED(data_in_reg), data_out_reg_shift'length))), ((SUB_IND_DATA_POS + TO_INTEGER(UNSIGNED(data_len_reg_shift))) - transmit_count) * DBIT));
						elsif(transmit_count < SUB_IND_DATA_POS + TO_INTEGER(UNSIGNED(data_len_reg_shift)) + SUM_DATA + 1) then
						  --компоновка контрольной суммы
						  sum_next <= STD_LOGIC_VECTOR(SHIFT_LEFT(UNSIGNED(sum_reg_shift or STD_LOGIC_VECTOR(resize(UNSIGNED(data_in_reg), sum_reg_shift'length))), ((SUB_IND_DATA_POS + TO_INTEGER(UNSIGNED(data_len_reg_shift)) + SUM_DATA) - transmit_count) * DBIT));
						  if(transmit_count = SUB_IND_DATA_POS + TO_INTEGER(UNSIGNED(data_len_reg_shift)) + SUM_DATA) then
							 stp_data  := err;       -- Переключение состаяния в режим проверки контрольной суммы
						  end if;
						end if;
						if(transmit_count < SUB_IND_DATA_POS + UNSIGNED(data_len_reg_shift) + 1) then
							check_sum	:= STD_LOGIC_VECTOR(UNSIGNED(check_sum) + resize(UNSIGNED(data_in_reg), check_sum'length));
						end if;
						transmit_count := transmit_count + t_incr; -- Инкремент счетчика принятых данных
					end if;
				when err =>
					if(sum_next /= (not check_sum)) then
						err_check_sum_next <= '1';
					end if;
					stp_data      := complit;             -- Переключение состаяния в режим завершения приема команды
					flag_next     <= '1';                 -- Установка флага завершения приема команды
					dev_id_reg    <= dev_id_reg_shift;
					cmd_id_reg    <= cmd_id_reg_shift;
					cmd_reg       <= cmd_reg_shift;
					data_len_reg  <= data_len_reg_shift;
					index_reg     <= index_reg_shift;
					sub_index_reg <= sub_index_reg_shift;
					data_out_reg  <= data_out_reg_shift;
				when complit =>
					--if(clr_flag = '1') then
					stp_data := init;
					--end if;
			end case;
		end if;
	end process;
end cmd_request;