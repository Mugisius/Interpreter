module Main (main) where

-- Домашнее задание по языку Хаскель. Акинин Евгений 525 группа.
-- Вариант: Интерпретатор языка Befunge-93
--                        (https://en.wikipedia.org/wiki/Befunge)
--
-- Код на Befunge представляет из себя двумерную таблицу со сшитыми
-- краями, по которой в различных направлениях перемещается интерпретатор,
-- исполняя команды, расположенные в её ячейках.
--
-- Основная логика программы (моей, на Хаскеле) - выполнение шага
-- инерпретатора в функции exec - выбор, с помощью сопоставления
-- шаблону и охранным выражениям, необходимой для выполнения команды;
-- изменение состояния интерпретатора (тип StateType) и рекусривый 
-- вызов exec
--
-- Обработку ошибок внутри кода на Befunge сделал на базовом уровне,
-- но при попытках взять из стека больше чем надо и при неправильных 
-- командах выдаётся соответствующее сообщение.


import Types


-- Как я понял, модуль Random не поддерживается в Haskell2010,
-- поэтому пришлось писать алтернативу через Hashable
import Data.Hashable
import Data.Time.Clock

import Data.Char

-- Имя файла с кодом для выполнения
codePath :: FilePath
codePath = "fib.txt"            -- Печатает первые 27 чисел Фиббоначи
--codePath = "HelloWorld.txt"   -- Hello World
--codePath = "numbers.txt"      -- Последовательность слегка случайных чисел
--codePath = "DNA.txt"          -- Выводит строчку из случайных символов ACGT
--codePath = "testFail1.txt"    -- <
--codePath = "testFail2.txt"    -- < Проверка вывода ошибок
--codePath = "testFail3.txt"    -- <

-- Остаток от деления с изменной логикой для отрицательных чисел
-- (чтобы получить кольцо вычетов)
myMod :: Int -> Int -> Int
myMod x y
    | x < 0 = (x + y) `myMod` y
    | otherwise = x `mod` y

-- Получение случайного направления, и именения состояния рандомизатора
rndDir :: Int -> (Direction, Int)
rndDir num
    | num `mod` 4 == 1 = (DUp, new_rnd)
    | num `mod` 4 == 2 = (DDown, new_rnd)
    | num `mod` 4 == 3 = (DLeft, new_rnd)
    | num `mod` 4 == 0 = (DRight, new_rnd)
        where new_rnd = abs $ hash $ show $ (num-123)

-- "Продвижение по коду" в нужном направлении
move :: Direction -> StateType -> StateType
-- (Почему именно так перемещается см. в комментарии в Types.hs)
move DUp state@(State _ _ _ _ c x _) = state{ s_code = (last c : init c), s_x = (x-1) `myMod` (length c) }
--                                            ^ изменение состояния кода  ^ изменение координаты интерпретатора
move DDown state@(State _ _ _ _ (c:tc) x _) = state{ s_code = (tc ++ [c]), s_x = (x+1) `myMod` (length (c:tc)) } 
move DLeft state@(State _ _ _ _ (c:tc) _ y) = state{ s_code = (map (\ i -> (last i : init i)) (c:tc)), s_y = (y-1) `myMod` (length c) } 
move DRight state@(State _ _ _ _ (c:tc) _ y) = state{ s_code = (map (\ (i:ti) -> (ti ++ [i])) (c:tc)), s_y = (y+1) `myMod` (length c) }
move _ c = c

-- Заменить символ в коде по координатам x y 
-- на значение val
put :: Int -> Int -> Int -> StateType -> StateType
put x y val state@(State _ _ _ _ ((_:tx):ty) val_x val_y) 
    | x == val_x && y == val_y = state{ s_code = (((chr val):tx):ty) }  -- Если на координатах интерпретатора, то меняем
    | x == val_x = move DLeft $ put x y val $ move DRight state         -- Иначе движемся по оси, меняем и движемся обратно
    | otherwise = move DDown $ put x y val $ move DUp state

-- Получить код символа на координатах x y
get :: Int -> Int -> StateType -> Int
get x y state@(State _ _ _ _ ((val:_):_) val_x val_y) 
    | x == val_x && y == val_y = ord val
    | x == val_x = get x y $ move DRight state
    | otherwise = get x y $ move DUp state

-- Сменить режим
cngMode :: Mode -> Mode
cngMode ModeNormal = ModeSymbol
cngMode ModeSymbol = ModeNormal

-- Основная функция, выполняющая очередной шаг программы
-- Возвращаемое значение: строчка для печати
exec :: StateType -> [Char]
exec (State _ _ [] _ _ _ _) = "\n\t<error>\n\nПопытка обращения к пустому стеку\n"                        -- Вывод ошибки обращения к пустому стеку
exec state@(State dir mode _ _ (('"':_):_) _ _) = exec $ move dir state{ s_mode = (cngMode mode) }              -- Смена режима
exec state@(State dir ModeSymbol stack _ ((ch:_):_) _ _) = exec $ move dir state{ s_stack = ((ord ch):stack) }  -- Заполнение стека в символьном режиме
exec state@(State _ _ (s:ts) _ (('_':_):_) _ _)         -- Смена направления в зависимости от значения на вершине стека
    | s == 0 = exec $ move DRight state{ s_dir = DRight, s_stack = ts }
    | otherwise = exec $ move DLeft state{ s_dir = DLeft, s_stack = ts }
exec state@(State _ _ (s:ts) _ (('|':_):_) _ _)         -- Смена направления в зависимости от значения на вершине стека
    | s == 0 = exec $ move DDown state{ s_dir = DDown, s_stack = ts }
    | otherwise = exec $ move DUp state{ s_dir = DUp, s_stack = ts }
exec state@(State dir _ (s:ts) rnd ((c:_):_) _ _)       -- Команды, которым нужно одно значение с вершины стека
    | c == ' ' = exec $ move dir state                          -- Продолжить движение
    | c == ':' = exec $ move dir state{ s_stack = (s:s:ts) }    -- Дублировать вершину стека
    | c == '$' = exec $ move dir state{ s_stack = ts }          -- Отбросить вершину стека
    | c == '#' = exec $ move dir $ move dir state               -- Перепрыгнуть следущую ячейку
    | c == '^' = exec $ move DUp state{ s_dir = DUp }           -- Двигаться вверх
    | c == 'v' = exec $ move DDown state{ s_dir = DDown }       -- Двигаться вниз
    | c == '<' = exec $ move DLeft state{ s_dir = DLeft }       -- Двигаться влево
    | c == '>' = exec $ move DRight state{ s_dir = DRight }     -- Двигаться вправо
    | c == '?' = exec $ move newDir state{ s_dir = newDir, s_rnd = new_rnd }    -- Двигаться в случайном направлении
    | c == '.' = (show s) ++ (exec $ move dir state{ s_stack = ts }) -- Распечатать вершину стека как число
    | c == ',' = (chr s) : (exec $ move dir state{ s_stack = ts })   -- Распечатать символ с кодом на вершине стека 
    | c == '@' = ""                                             -- Конец программы
        where (newDir, new_rnd) = rndDir rnd
exec state@(State dir _ (s2:s1:ts) _ ((c:_):_) _ _)         -- Команды, которым нужно два значения с вершины стека
    | c == '*' = exec $ move dir state{ s_stack = (s1*s2:ts) }       -- Перемножить значения на вершине стека
    | c == '+' = exec $ move dir state{ s_stack = (s1+s2:ts) }       -- Сложить значения на вершине стека
    | c == '-' = exec $ move dir state{ s_stack = (s1-s2:ts) }       -- Вычесть значения на вершине стека
    | c == '/' = exec $ move dir state{ s_stack = (s1 `div` s2:ts) } -- Разделить значения на вершине стека
    | c == '%' = exec $ move dir state{ s_stack = (s1 `mod` s2:ts) } -- Найти остаток от деления значений на вершине стека
    | c == '\\' = exec $ move dir state{ s_stack = (s1:s2:ts) }      -- Поменять местами значения на вершине стека
exec state@(State dir _ (s:ts) _ (('!':_):_) _ _)           -- Поместить в стек логическое отрицание значения на его вершине
    | s == 0 = exec $ move dir state{ s_stack = (1:ts) }
    | otherwise = exec $ move dir state{ s_stack = (0:ts) }
exec state@(State dir _ (a:b:ts) _ (('`':_):_) _ _)         -- Поместить в стек результат сравнения значений его вершин
    | b > a = exec $ move dir state{ s_stack = (1:ts) }
    | otherwise = exec $ move dir state{ s_stack = (0:ts) }
exec state@(State dir _ (y:x:v:ts) _ (('p':_):_) _ _) = exec $ move dir $ put x y v state{ s_stack = ts }       -- Заменить символ в коде на нужное значени
exec state@(State dir _ (y:x:ts) _ (('g':_):_) _ _) = exec $ move dir $ state{ s_stack = (get x y state):ts }   -- Поместить в стек значение сивола кода
exec state@(State dir _ stack _ ((ch:_):_) _ _)
        | isDigit ch = exec $ move dir state{ s_stack = ((digitToInt ch):stack) }    -- Для чисел: поместить его на вершину стека
exec (State _ _ _ _ ((ch:_):_) _ _) = "\n\t<error>\nНедопустимый символ '" ++ [ch] ++ "' или обращение к пустому стеку\n\n"
exec (State _ _ _ _ [] _ _) = "\n\t<error>\nПустой входной файл\n\n"
exec _ = "\n\t<error>\n\n"


main :: IO ()
main = do
    rnd_raw <- getCurrentTime   -- Задаём начальное состояние рандомизатора
    ans <- readFile codePath    -- Считываем код
    putStr $ exec (State DRight ModeNormal [0] (abs $ hash $ show $ rnd_raw) (lines ans) 0 0)   -- Запускаем интерпретатор