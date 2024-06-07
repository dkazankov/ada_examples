pragma Ada_95;

with Ada.Text_IO;

procedure Weird_Numbers is

   use Ada.Text_IO;
   package Positive_IO is new Integer_IO(Num => Positive);
   use Positive_IO;

   type Positive_Array is array (Positive range <>) of Positive;

   function Append(A: Positive_Array; V: Positive) return Positive_Array is
      R: Positive_Array(A'First..A'Last+1);
   begin
      for I in A'Range loop
         R(I) := A(I);
      end loop;
      R(R'Last) := V;
      return R;
   end Append;

   function Prepend(V: Positive; A: Positive_Array) return Positive_Array is
      R: Positive_Array(A'First..A'Last+1);
   begin
      R(R'First) := V;
      for I in A'Range loop
         R(I+1) := A(I);
      end loop;
      return R;
   end Prepend;

   function Proper_Divisors (Value: Positive) return Positive_Array is
      function Inner(Factor: Positive) return Positive_Array is
         Factor2: constant Positive := Factor*Factor;
      begin
         if Factor2 < Value then
            if Value rem Factor = 0 then
               return Prepend(Value/Factor, Append(Inner(Factor+1), Factor));
            else
               return Inner(Factor+1);
            end if;
         elsif Factor2 = Value then
            return (1 => Factor);
         end if;
         return (1..0 => 1); -- Empty
      end Inner;
   begin
      if Value = 1 then
         return (1 => 1);
      end if;
      return Append(Inner(2), 1);
   end Proper_Divisors;

   function Sum (Set: Positive_Array) return Natural is
      S: Natural := 0;
   begin
      for I in Set'Range loop
         S := S + Set(I);
      end loop;
      return S;
   end Sum;

   function Subset_Sum_Exists (Set: Positive_Array; Sum: Positive) return Boolean is
      function Inner (First: Positive; Sum: Positive) return Boolean is
         Value: Positive;
      begin
         if First > Set'Last then
            return False;
         end if;
         Value := Set(First);
         if Sum = Value then
            return True;
         elsif Sum < Value then
            return Inner(First+1, Sum);
         else
            return Inner(First+1, Sum-Value) OR else Inner(First+1, Sum);
         end if;
      end Inner;
   begin
      return Inner(Set'First, Sum);
   end Subset_Sum_Exists;

   Number: Positive := 2;
   Numbers_Found: Natural := 0;
   Abundant, Semiperfect, Weird: Boolean;
begin
   Put ("The first 25 weird numbers are:");
   while Numbers_Found < 25 loop
      declare
         Divisors: Positive_Array := Proper_Divisors (Number);
      begin
         Abundant := Sum (Divisors) > Number;
         if Abundant then
            Semiperfect := Subset_Sum_Exists (Divisors, Number);
            Weird := NOT Semiperfect;
            if Weird then
               Put (" ");
               Put (Number, 0);
               Numbers_Found := Numbers_Found + 1;
            end if;
         end if;
      end;
      Number := Number + 1;
   end loop;
   New_Line;
end Weird_Numbers;
