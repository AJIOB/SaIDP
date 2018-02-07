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
xdots := seq(a .. b, (b - a) / N):
ydots := seq(y(x), x in xdots):

printf("Исходный график");
plot([xdots], [ydots]);

# Глобальный счетчик итераций
globalCounterSum := 0:
globalCounterMul := 0:


# Made by AJIOB
printf("## Дискретное преобразование Фурье\n");

