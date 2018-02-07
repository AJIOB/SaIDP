printf("Исходная функция:");
y := x -> sin(3*x)+cos(x);

# Период
T := 2 * Pi:

a := 0:
b := evalf(a + T):

plot(y(x), x=a..b);

printf("T = %f\n", evalf(T));

# Число точек
N := 16:
printf("N = %d\n", N);

W := exp(-I*2*Pi/N):

# Точки для ПФ
dotsDelta := (b-a)/N:
xdots := seq(dotsDelta * x, x = 0 .. N):
ydots := seq(y(x), x in xdots):

printf("Исходный график после дискретизации");
plot([xdots], [ydots]);

# Глобальный счетчик итераций
globalCounterSum := 0:
globalCounterMul := 0:

numSeq := seq(k, k = 0..(N - 1)):

# Made by AJIOB
printf("## Дискретное преобразование Фурье\n");

universalFunction := (k, sequence, WSignPower) -> simplify(1/N * sum(sequence[m+1]*W^(k * m * (-1)^WSignPower), m = 0 .. N-1)):

complexSignalFunc := k -> universalFunction(k, [ydots], 0):
yComplexDots := seq(complexSignalFunc(k), k in numSeq):

yComplexAmplitudes := seq(abs(d), d in yComplexDots):

printf("# Амплитудно-частотный спектр\n");
plot([numSeq], [yComplexAmplitudes]);

printf("# Фазо-частотный спектр\n");
#plot([numSeq], [yComplexDots]);
