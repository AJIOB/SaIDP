printf("Исходная функция:");
printf("\ny:");
y := x -> sin(3*x);
printf("z:");
z := x -> cos(x);

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

# Опорные точки
numSeq := seq(k, k = 0..(N - 1)):

# Точки для ПФ
dotsDelta := (b-a)/N:
xdots := seq(dotsDelta * x, x in numSeq):
ydots := seq(y(x), x in xdots):
zdots := seq(z(x), x in xdots):

#Генерит индексы для сортировки элементов в БПФ
prName := proc (arr, k)::Array;
  if k < 1 then
    return arr;
  end if;
  return prName([op(arr), seq(k + i, i in arr)], k / 2);
end proc:

#sorted_indexes := prName([0], N/2):

# Made by AJIOB
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

FFT_custom := proc (arr)::Array;
  local buff := butterfly(arr, nops(arr), 0);
  return [seq(buff[i + 1] / N, i in prName([0], nops(arr)/2))]:
end proc:

FFT_custom_inverse := proc (arr)::Array;
  local buff := butterfly(arr, nops(arr), 1);
  return [seq(buff[i + 1], i in prName([0], nops(arr)/2))]:
end proc:
