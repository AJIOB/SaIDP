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

universalFunction := (k, sequence, WSignPower) -> simplify(1/N * sum(sequence[m+1]*W^(k * m * (-1)^WSignPower), m = 0 .. N-1)):

complexSignalFunc := k -> universalFunction(k, [ydots], 0):
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

#Генерит индексы
prName := proc (k, arr)::Array; 
	local arr1, arrTemp; 
	arr1 := arr; 
	arrTemp := Array([op(arr)]);
  local i := 0;
  local retArr := Array([]);
	if k < 1 then 
		retArr := arr1 
	else 
		for i to ArrayNumElems(arrTemp) do 
			arr1[i] := arr1[i]+k 
		end do; 
		retArr := prName((1/2)*k, [op(arr), op(arr1)]) 
	end if; 
	return retArr 
end proc;

#БПФ с прореживанием по частоте
butterfly := proc (n, arr)::Array; 
  	local w, Wn, b, c, aB, aC, arr1, arr2, i; 
 	w := 1; 
 	Wn := exp((I*2)*Pi/N); 
 	b := 0; 
 	c := 0; 
  	aB := []; 
  	aC := []; 
  	arr1 := []; 
 	arr2 := []; 
 	for i to (1/2)*n do 
   		b := arr[i]+arr[i+(1/2)*n]; 
    	aB := [op(aB), b]; 
    	c := (arr[i]-arr[i+(1/2)*n])*w; 
    	aC := [op(aC), c]; 
    	w := w*Wn; 
  	end do; 

  	if (1/2)*n = 1 then 
    	arr1 := aB; 
    	arr2 := aC; 
  	else 
    	arr1 := butterfly((1/2)*n, aB); 
    	arr2 := butterfly((1/2)*n, aC); 
  	end if; 
  	return [op(arr1), op(arr2)];
end proc;