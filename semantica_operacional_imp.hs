module SemOpIMP where

-- tipos usados por este módulo
type Loc = String	-- este tipo é como uma variável, no caso a String que ele retorna é o nome da variável
type Env = String -> Int 	-- este tipo é como um mapeamento, mapeia o nome (String) da variável (no caso o Loc) para um Int
type Bool_A = (Bool, Int)	-- para recuprar use fst e snd
type Bool_B = (Bool, Bool)

-- expressões para utilizar operações na linguagem de programação
data AExp = N Int | L Loc | ADD AExp AExp | SUB AExp AExp | MULT AExp AExp
data BExp = B Bool | NOT BExp | EQUALS AExp AExp | LESSEQUAL AExp AExp | AND BExp BExp
-- sintaxe para construir programas usando as operações acima
data Prg = SKIP | ASSG Loc AExp | IFELSE BExp Prg Prg | WHILE BExp Prg | REPEATUNTIL Prg  BExp | SEQ Prg Prg

-- impressões para uma impressão agradável
instance Show AExp
	where
		show (N i) = show i
		show (L l) = show l
		show (ADD a0 a1) = "("++ show a0 ++"+"++ show a1 ++ ")"
		show (SUB a0 a1) = "("++ show a0 ++"-"++ show a1 ++ ")"
		show (MULT a0 a1) = "("++ show a0 ++"*"++ show a1 ++ ")"

instance Show BExp
	where
		show (B b) = show b
		show (NOT b) = "¬("++ show b ++ ")"
		show (EQUALS b0 b1) = "("++ show b0 ++"=="++ show b1 ++ ")"
		show (AND b0 b1) = "("++ show b0 ++"&&"++ show b1 ++ ")"

instance Show Prg
	where
		show (SKIP) = "skip"
		show (ASSG l a) = show l ++ ":=" ++ show a
		show (IFELSE b prg0 prg1) = "if" ++ show b ++ "{" ++ show prg0++"} else {" ++ show prg1 ++ "}"
		show (WHILE b prg) = "while" ++ show b ++ "{" ++ show prg ++"}"
		show (REPEATUNTIL prg b) = "repeat {" ++ show prg ++ "} until" ++ show b
		show (SEQ prg0 prg1) = show prg0 ++ ";;" ++ show prg1


-- memória que é utilizada nos programas deste módulo, mapeando variável |-> valor
env :: Env
env "x0" = 5
env "y0" = 2
env "x2" = 4
env "y2" = 10
env "product" = 1 	-- variável de prog1
env "sum" = 0		-- variável de prog2 e prog3
env "times" = 0		-- variável de prog1, prog2 e prog3
env "n0" = 0		-- variável de prog4
env "m0" = 0		-- variável de prog4
env "q" = 0			-- variável de prog4
env _ = 0

-- atualiza a variável (Loc) com um novo valor (Int) na memória (Env), retornando a memória atualizada.
update_env :: (Env, Loc, Int) -> Env
update_env (env, loc, value) = (\loc'-> if loc==loc' then value else env loc')

-- avaliador de expressões aritméticas; o 'chute' (um resultado esperado) serve para verificar se as operações (individualmente) funcionam corretamente, por isso são dados 'chutes' quaisquer em expressões encadeadas
a_eval :: AExp -> (Env -> (Int -> Bool_A))
a_eval (N x) env n = (n == x, x)	-- retorna o valor do Int em uma tupla, sendo a primeira posição a verificação se o 'chute' é o mesmo valor na memória
a_eval (L x) env n = (n == env x, env x)	-- retorna o valor da variável em uma tupla, sendo a primeira posição a verificação se o 'chute' equivale ao valor da variável mapeada em memória
a_eval (ADD a_exp0 a_exp1) env n = (n1+n2 == n, n1 + n2)	-- retorna a soma de duas expressões, sendo a primeira posição a verificação se a soma das subexpressões equivalem ao 'chute'
	where	-- avaliação das subexpressões
		n1 = snd (a_eval a_exp0 env n)
		n2 = snd (a_eval a_exp1 env n)
a_eval (SUB a_exp0 a_exp1) env n = (n1-n2 == n, n1 - n2)	-- retorna a subtrãção de duas expressões em uma tupla, sendo a primeira posição a verificação se a subtração das subexpressões equivalem ao 'chute'.
	where	-- avaliação das subexpressões
		n1 = snd (a_eval a_exp0 env n)
		n2 = snd (a_eval a_exp1 env n)
a_eval (MULT a_exp0 a_exp1) env n = (n1*n2 == n, n1*n2)		-- retorna a multiplicação de duas expressões em uma tupla, sendo a primeira posição a verificação se a multiplicação das subexpressões equivalem ao 'chute'
	where	-- avaliação das subexpressões
		n1 = snd (a_eval a_exp0 env n)
		n2 = snd (a_eval a_exp1 env n)

-- avaliador de expressões booleanas; o 'chute' (um resultado esperado) serve para verificar se as operações (individualmente) funcionam corretamente, por isso são dados 'chutes' quaisquer em expressões encadeadas
b_eval :: BExp -> (Env -> (Bool -> Bool_B))
b_eval (B x) env b = (b == x, x)	-- retorna o valor booleano em uma tupla, sendo a primeira posição a verificação se o 'chute' é o mesmo que o valor
b_eval (NOT b_exp) env b = (b == b1, b1)	-- retorna a negação de uma expressão em uma tupla, sendo a primeira a verificação se o 'chute' equivale a negação da expressão
	where	-- avaliação das subexpressões
		b1 = not (snd (b_eval b_exp env b))
b_eval (EQUALS a_exp0 a_exp1) env b = ((a1==a2) == b, a1==a2)	-- retorna se duas expressões aritméticas são equivalentes em uma tupla, sendo a primeira posição a verificação se o valor da igualdade é o mesmo que o 'chute'
	where	-- avaliação das subexpressões
		a1 = snd (a_eval a_exp0 env 0)	-- captura o resultado de a_exp0 dando um chute qualquer, pois interessa apenas o resultado retornado
		a2 = snd (a_eval a_exp1 env 0)	-- captura o resultado de a_exp1 dando um chute qualquer, pois interessa apenas o resultado retornado
b_eval (LESSEQUAL a_exp0 a_exp1) env b = ((a1<=a2) == b, a1<=a2)	-- retorna se uma expressão aritmética é menor ou igual à outra em uma tupla, sendo a primeira posição a verificação se o valor da comparação é a mesma que o 'chute'
	where	-- avaliação das subexpressões
		a1 = snd (a_eval a_exp0 env 0)	-- captura o resultado de a_exp0 dando um chute qualquer, pois interessa apenas o resultado retornado
		a2 = snd (a_eval a_exp1 env 0)	-- captura o resultado de a_exp1 dando um chute qualquer, pois interessa apenas o resultado retornado
b_eval (AND b_exp0 b_exp1) env b = (b == (b1 && b2), b1 && b2)	--	retorna a operação de conjunção entre duas expressões em uma tupla, sendo a primeira posição a verificação se a operação equivale ao 'chute'
	where	-- avaliação das subexpressões
		b1 = snd (b_eval b_exp0 env b)
		b2 = snd (b_eval b_exp1 env b)

-- avaliador de programas; neste avaliador tem as operações simples que uma linguagme de programação dispõe, permitinido criar programinhas simples
p_eval :: Prg -> (Env -> Env)
p_eval (SKIP) env = env 	-- retorna o mesmo environment que recebeu
p_eval (ASSG loc a_exp) env = update_env (env, loc, n)	-- operação de atribuição de uma expressão artimética à uma variável (Loc) em uma dada memória (Env)
	where
		n = snd (a_eval a_exp env 0)	-- captura o resultado de a_exp dando um chute qualquer, pois interessa apenas o resultado retornado
p_eval (SEQ prg0 prg1) env = p_eval prg1 new_env	-- sequências de programas; isto é, executa prg1 após prg0 tiver sido executado e ter modificado o environment
	where
		new_env = p_eval prg0 env 	-- memória atualizada após a execução do primeiro programa
p_eval (IFELSE b_exp prg0 prg1) env 	-- estrutura de controle de execução simples, avalia se a expressão booleana for verdadeira então executa o primeiro programa, senão o segundo
	| (snd (b_eval b_exp env False)) = p_eval prg0 env	-- se a condição for verdadeira, executa recursivamente o prg0
	| otherwise = p_eval prg1 env	-- senão, executa recursivamente o prg1
p_eval (WHILE b_exp prg) env = while b_exp prg env 	-- estrutura de repetição 'enquanto', avalia se a expressão booleana for verdadeira numa dada memória executa repetidamente um programa que modifica a mesma memória 
	where
		while :: BExp -> Prg -> Env -> Env
		while b_exp prg env
			| (snd (b_eval b_exp env False)) = while b_exp prg new_env		-- se a condição for verdadeira, executa o prg com o environment atualizado
			| otherwise = env 	-- senão, retorna o mesmo environment
				where
					new_env = p_eval prg env


-- ====== EXPRESSÕES PARA SEREM EXECUTADAS ========================
-- fatorial de 'n'
prog1 :: Int -> Int
prog1 n 
	| n >= 0 = new_env "product"	
		where
			new_env = p_eval (SEQ (ASSG "times" (N 1)) ((WHILE (LESSEQUAL (L "times") (N n)) (SEQ (ASSG "product" (MULT (L "product") (L "times"))) (ASSG "times" (ADD (L "times") (N 1))))))) env

-- multiplicação: somas sucessivas de 'm' 'n' vezes
prog2 :: Int -> Int -> Int
prog2 n m
	| (n >= 0) && (m >= 0) = new_env "sum"
		where
			new_env = p_eval (SEQ ((ASSG "times" (N n))) (WHILE (NOT (EQUALS (L "times") (N 0))) (SEQ (ASSG "sum" (ADD (L "sum") (N m))) (ASSG "times" (SUB (L "times") (N 1)))))) env

-- divisão: subtrações sucessivas de 'n' 'd' vezes
prog3 :: Int -> Int -> Int
prog3 n d
	| (n >= 0) && (d > 0) = new_env "sum"	-- o divisor não pode ser 0
		where
			new_env = p_eval (SEQ (ASSG "times" (N n)) (WHILE (LESSEQUAL (N 0) (SUB (L "times") (N d))) (SEQ (ASSG "sum" (ADD (L "sum") (N 1))) (ASSG "times" (SUB (L "times") (N d)))))) env

-- MDC entre 'n' e 'm'
prog4 :: Int -> Int -> Int
prog4 n m
	| (n >= 0) && (n >= 0) = new_env "n0"
		where
			new_env = p_eval (SEQ (ASSG "n0" (N n)) (SEQ (ASSG "m0" (N m)) (WHILE (NOT (EQUALS (L "m0") (N 0))) (SEQ (ASSG "q" (L "n0")) (SEQ (WHILE (LESSEQUAL (L "m0") (L "q")) (ASSG "q" (SUB (L "q") (L "m0")))) (SEQ (ASSG "n0" (L "m0")) (ASSG "m0" (L "q")))))))) env

teste01 = a_eval (MULT (ADD (L "x0") (L "y0")) (N 4)) env 28	-- deve retornar True
teste02 = b_eval (NOT (LESSEQUAL (ADD (L "x0") (L "y0")) (ADD (N 1) (L "y0")))) env True	-- deve retornar True
teste03 = a_eval (ADD (L "x2") (L "y2")) env 10 -- deve retornar False
teste04 = b_eval (NOT (B False)) env True -- deve retornar True
teste05 = b_eval (EQUALS (SUB (N 10) (L "x2")) (ADD (L "y0") (N 4))) env True	-- deve retornar True
-- ================================================================


{-
Algoritmo de prog1:
	int fact(int n) {
		int product = 1, times = 1;
		while (times <= n) {
			product = product * times;
			times = times + 1;
		}
		return product;
	}

Algoritmo de prog2:
	int mult(int n, int m) {
	    int sum = 0, times = n;
	    while (!(times==0)) {
	        sum = sum + m;
	        times = times - 1;
	    }
	    return sum;
	}

Algoritmo de prog3:
	int div(int n, int d) {
	    int sum = 0, times = n;
	    while (0 <= times - d) {
	        sum = sum + 1;
	        times = times - d;
	    }
	    return sum;
	}

Algoritmo de prog4:
	int mdc(int n, int m) {
	    int n0 = n, m0 = m;
	    while(!(m0==0)) {
	        int q = n0;
	        while(m0 <= q)
	            q = q-m0;
	        n0=m0;
	        m0=q;
	   }
	   return n0;
	}
-}
