module Types where

-- Направление движение интерпритатора по коду
data Direction = DUp | DDown | DLeft | DRight
    deriving (Eq, Show, Read)

-- Режим интерпритатора: Обычный или Символьный
data Mode = ModeNormal | ModeSymbol
    deriving (Eq, Show, Read)

-- Состояние интерпритатора на определенном шаге
data StateType = State
    { s_dir :: Direction    -- Направление его движения
    , s_mode :: Mode        -- Режим интерпритатора
    , s_stack :: [Int]      -- Стек
    , s_rnd :: Int          -- Состояние рандомизатора
    , s_code :: [[Char]]    -- Текущее состояние кода*
    , s_x :: Int            -- Текущая координата x 
    , s_y :: Int }          -- Текущая координата y 
    deriving (Eq, Show, Read)

-- *- вместо того, чтобы "перемещаться" по коду исполнителем
-- будем менять код, таким образом, чтобы каждый раз очередной
-- символ команды, которую необходимо выполнить был первым в
-- первой строчке. Таким образом легко выявлять символ команды 
-- для выполения паттерном (comm:_):_