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

# Опорные точки
numSeq := seq(k, k = 0..(N - 1)):

printf("Исходный график после дискретизации");
plot([numSeq], [ydots]);

# Глобальный счетчик итераций
globalCounterSum := 0:
globalCounterMul := 0:

# Made by AJIOB
printf("## Дискретное преобразование Фурье\n");

universalFunction := (k, sequence, WSignPower) -> simplify(sum(sequence[m+1]*W^(k * m * (-1)^WSignPower), m = 0 .. N-1)):

complexSignalFunc := k -> (universalFunction(k, [ydots], 0) / N):
yComplexDots := seq(complexSignalFunc(k), k in numSeq):

printf("# Амплитудно-частотный спектр\n");
yComplexAmplitudes := seq(abs(d), d in yComplexDots):
plot([numSeq], [yComplexAmplitudes]);

printf("# Фазо-частотный спектр\n");
yComplexAngles := seq(argument(d), d in yComplexDots):
plot([numSeq], [yComplexAngles]);

printf("# Восстановленный сигнал\n");
recoveredSignalFunc := k -> universalFunction(k, [yComplexDots], 1):
yRecoveredDots := seq(Re(recoveredSignalFunc(k)), k in numSeq):
plot([numSeq], [yRecoveredDots]);

printf("## Быстрое преобразование Фурье с прореживанием по частоте\n");

#Генерит индексы
prName := proc (arr, k)::Array;
  if k < 1 then
    return arr;
  end if;
  return prName([op(arr), seq(k + i, i in arr)], k / 2);
end proc:

#БПФ с прореживанием по частоте
butterfly := proc (arr, n, dir)::Array; 
  local w := 1;
  local Wn := exp(((-1) ^ dir) *2 * I * Pi / n); 
  local b := []; 
  local c := []; 
  local i;
  if nops(arr) < 2 then
    return arr;
  end if;
  for i from 1 by 1 to (n/2) do 
    b := [op(b), arr[i]+arr[i+(n/2)]]; 
    c := [op(c), (arr[i]-arr[i+(n/2)])*w]; 
    w := w*Wn; 
  end do; 
  return [op(butterfly(b, n / 2, dir)), op(butterfly(c, n / 2, dir))];
end proc:

yFFTComplexDotsTemp := butterfly([ydots], N, 0):
yFFTComplexDots := seq(yFFTComplexDotsTemp[i + 1], i in prName([0], N/2)):

printf("# Амплитудно-частотный спектр\n");
yFFTComplexAmplitudes := seq(abs(d), d in yFFTComplexDots):
plot([numSeq], [yFFTComplexAmplitudes]);

printf("# Фазо-частотный спектр\n");
yFFTComplexAngles := seq(argument(d), d in yFFTComplexDots):
plot([numSeq], [yFFTComplexAngles]);

printf("# Восстановленный сигнал\n");
yFFTRecoveredDotsTemp := butterfly([yFFTComplexDots], N, 1):
yFFTRecoveredDots := seq(Re(yFFTRecoveredDotsTemp[i + 1]), i in prName([0], N/2)):
plot([numSeq], [yFFTRecoveredDots]);
