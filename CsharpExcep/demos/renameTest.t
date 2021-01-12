  $ (cd ../../../../default && demos/demoRenameTransformation.exe) 
  -_-_-_-_-_-_-_-_-_-_- First rename test -_-_-_-_-_-_-_-_-_-_-
  
  -- public int GetFact(int n)
  ++ public int GetFact(int num)
  
  -- if (n <= 1)
  ++ if (num <= 1)
  
  -- return n * GetFact(n - 1);
  ++ return num * GetFact(num - 1);
  
  -_-_-_-_-_-_-_-_-_-_- Second rename test -_-_-_-_-_-_-_-_-_-_-
  
  -- throw e;
  ++ throw excep;
  
  -- catch (ShittyExn e) when (false)
  ++ catch (ShittyExn excep) when (false)
  
  -- catch (ShittyExn e) when (false)
  ++ catch (ShittyExn excep) when (false)
  
  -- catch (ShittyExn e) when (false)
  ++ catch (ShittyExn excep) when (false)
  
  -- Console.WriteLine(e);
  ++ Console.WriteLine(excep);
  
  -- catch (ShittyExn e) when (e.Filter())
  ++ catch (ShittyExn excep) when (excep.Filter())
  
  -_-_-_-_-_-_-_-_-_-_- Third rename test -_-_-_-_-_-_-_-_-_-_-
  
  -- int a = 10, b = 50, c = 100;
  ++ int magic = 10, b = 50, c = 100;
  
  -- if (b > a)
  ++ if (b > magic)
  
  -- if (a < c)
  ++ if (magic < c)
  
  -- if (d == a)
  ++ if (d == magic)
  
  -- if (a != b)
  ++ if (magic != b)
  
  -- if (a < b && b < c)
  ++ if (magic < b && b < c)
  
  -- if (a < b || a != 10)
  ++ if (magic < b || magic != 10)
  
  -- if (!(a >= c))
  ++ if (!(magic >= c))
  
 
