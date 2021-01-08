### An implementaion of Java-OOP mini-language

This is a homework for functional programming course.

License: LGPL

Author: Bozhnyuk Alexander, bozhnyuks@mail.ru

Замечания. 
- 1) Ключевые слова не могут являться именами классов
- 2) Метод main ничего в себя не принимает, является просто точкой входа программы. Метод main - единственный.
- 3) i++ и ++i эквиваленты i = i + 1. Ничего более
- 4) StackOverflow не детектится
- 5) Многомерные массивы не поддерживаются
- 6) Приведение типов через () не поддерживается. 
- 7) Метод equals при сравнении объектов вызывается в случае, если он переопределен. В противном случае происходит непосредственное сравнение ссылок.
- 8) Работа с реплом: запуск факториала 
    ```
    > int fac1(int acc, int n) {if (n<=1) return acc}; else return fac1(acc*n, n-1); }@
    > int fac(int n) {return fac(1, n)}@
    > fac(5)@
    > Result: VInt (120)
    ```
- 9) Работа с реплом: ArrayTypeMismatchException: 
    ```
    > Object[] x = new Object[3];@    
    Statement evaluated
    > x[0] = new Circle(5);@
    ArrayStoreException
    > Object[] y = new Figure[3];@
    Statement evaluated
    > y[0] = new QuickSorter();@
    Wrong assign type!

    ```

Features done:

- 1 Загрузка классов
- 2 Стандартные конструкции: ветвления, циклы
- 3 Стандартные типы: числа, строки и операции с ними
- 4 Стандартный тип массива и операции с ним.
- 5 ООП: классы, публичные методы, публичные поля.  
- 6 ООП: стандартный базовый класс Object
- 7 ООП: наследование
- 8 Рекурсия
- 9 Тесты, в том числе и тест паттерна Visitor, требуемый тест на массивы
- 10 REPL и набор классов для стандартной библиотеки.

Класс Object: 
```
public class Object {
    public int equals(Object obj) {
        if (this == obj) return 1;
        else return 0;
    }
    
    public String toString() {
    	return "Object";
    }
}
```

Features in progress:

- 1 Правки
- 2 
- 3 



