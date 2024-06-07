pragma Ada_95;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

procedure Two_Sum is

   subtype Index_Type is Positive;
   subtype Exteded_Index_Type is Index_Type'Base range Index_Type'First-1..Index_Type'Last;
   subtype Element_Type is Positive;
   type Element_Array is array (Index_Type range <>) of Element_Type;

   Ops: Natural;

   -- O(n * n) , memory O(n)
   procedure Two_Sum_n2 (Nums: Element_Array; Target: Element_Type; First, Second: out Exteded_Index_Type) is
   begin
      Ops := 0;
      for i in Nums'First..Nums'Last-1 loop -- O(n)
         for j in i+1..Nums'Last loop -- O(n)
               Ops := Ops + 1;
               if Nums(i) + Nums(j) = Target then
                  First := i;
                  Second := j;
                  return;
               end if;
         end loop;
      end loop;

      First := Exteded_Index_Type'First;
      Second := Exteded_Index_Type'First;
   end Two_Sum_n2;

   -- O(n * log n) , memory O(2 * n)
   procedure Two_Sum_nlogn (Nums: Element_Array; Target: Element_Type; First, Second: out Exteded_Index_Type) is

      Idx: array (Nums'Range) of Index_Type;

      -- Sort input nums in accending order O(n * log n)
      procedure Sort is
      begin
         Idx(Nums'First) := Nums'First;
         for i in Nums'First+1..Nums'Last loop -- O(n)
            Bubble_Loop : for j in reverse Idx'First..i-1 loop -- O(log n)
               Ops := Ops + 1;
               if Nums(i) >= Nums(Idx(j)) then
                  Idx(j+1) := i;
                  exit Bubble_Loop;
               else
                  Idx(j+1) := Idx(j);
                  Idx(j) := i;
               end if;
            end loop Bubble_Loop;
         end loop;
      end;

      -- Seek the target O(log n)
      procedure Seek is
         Left, Right: Index_Type;
         Sum: Element_Type;
      begin
         Left := Idx'First;
         Right := Idx'Last;
         while Left < Right loop -- O(log n)
            Ops := Ops + 1;
            Sum := Nums(Idx(Left)) + Nums(Idx(Right));
            if Sum = Target then
               if Idx(Left) < Idx(Right) then
                  First := Idx(Left);
                  Second := Idx(Right);
               else
                  Second := Idx(Left);
                  First := Idx(Right);
               end if;
               return;
            elsif Sum > Target then
               Right := Right-1;
            else -- Sum < Target
               Left := Left+1;
            end if;
         end loop;

         First := Exteded_Index_Type'First;
         Second := Exteded_Index_Type'First;
      end;
   begin
      Ops := 0;
      Sort;
      Seek;
   end Two_Sum_nlogn;

   -- O(n) , memory O(?)
   procedure Two_Sum_n (Nums: Element_Array; Target: Element_Type; First, Second: out Exteded_Index_Type) is

      type Map_Type is array (Element_Type range <>) of Exteded_Index_Type;
      type Map_Access is access Map_Type;
      subtype Cursor_Type is Exteded_Index_Type;

      procedure Free is new Ada.Unchecked_Deallocation(Map_Type, Map_Access);

      Diffs: Map_Access; -- Diffs is a very large (potentially) sparse array, so we need to allocate it in heap
      j: Cursor_Type;

      procedure Init is
         Min_Value, Max_Value: Element_Type := Nums(Nums'First);
         Diff: Element_Type;
      begin
         Ops := Ops + 1;
         Diff := Target - Nums(Nums'First);
         if Min_Value > Diff then
            Min_Value := Diff;
         end if;
         if Max_Value < Diff then
            Max_Value := Diff;
         end if;
         for i in Index_Type'Succ(Nums'First)..Nums'Last loop
            Ops := Ops + 1;
            if Min_Value > Nums(i) then
               Min_Value := Nums(i);
            end if;
            if Max_Value < Nums(i) then
               Max_Value := Nums(i);
            end if;
            Diff := Target - Nums(i);
            if Min_Value > Diff then
               Min_Value := Diff;
            end if;
            if Max_Value < Diff then
               Max_Value := Diff;
            end if;
         end loop;

         Diffs := new Map_Type(Min_Value..Max_Value);
      end Init;

      function Has_Element(I: Cursor_Type) return Boolean is
      begin
         return I /= Exteded_Index_Type'First;
      end Has_Element;

      function Element(I: Cursor_Type) return Index_Type is
      begin
         return I;
      end Element;

      function Find(E: Element_Type) return Cursor_Type is
      begin
         return Diffs.all(E);
      end Find;

      procedure Insert(E: Element_Type; I: Index_Type) is
      begin
         Diffs.all(E) := I;
      end Insert;
      pragma Inline (Has_Element, Element, Find, Insert);
   begin
      Ops := 0;
      Init;

      for i in Nums'Range loop -- O(n)
         Ops := Ops + 1;
         j := Find( Target - Nums(i) ); -- must be O(1)
         if Has_Element(j) then
            First := Element(j);
            Second := i;
            Free(Diffs);
            return;
         end if;

         Insert( Nums(i), i ); -- must be O(1)
      end loop;

      Free(Diffs);

      First := Exteded_Index_Type'First;
      Second := Exteded_Index_Type'First;
   end Two_Sum_n;

   package My_Integer_IO is new Integer_IO(Num => Element_Type);
   use My_Integer_IO;

   procedure Get (A: out Element_Array)
   is
      T : Element_Type;
   begin
      for I in A'Range loop
         Put("A("); Put(I, 0); Put("):=");
         Get(T);
         A(I) := T;
      end loop;
   end Get;

   procedure Put (A: Element_Array)
   is
   begin
      Put("[");
      for I in A'Range loop
         Put(A(I), 0);
         if I /= A'Last then
            Put(", ");
         end if;
      end loop;
      Put("]");
   end Put;

   procedure Execute(Nums: Element_Array; Target: Element_Type) is
      First, Second: Exteded_Index_Type;
   begin
      Put("O(n * n) , memory O(n): "); Put(Nums); New_Line;
      Two_Sum_n2(Nums, Target, First, Second);
      Put("Result: "); Put(First, 0); Put(","); Put(Second, 0); Put("; "); Put(Ops, 0); Put(" ops"); New_Line; -- 5,6

      Put("O(n * log n) , memory O(2 * n): "); Put(Nums); New_Line;
      Two_Sum_nlogn(Nums, Target, First, Second);
      Put("Result: "); Put(First, 0); Put(","); Put(Second, 0); Put("; "); Put(Ops, 0); Put(" ops"); New_Line; -- 5,6

      Put("O(n) , memory O(?): ");
      Two_Sum_n(Nums, Target, First, Second); Put(Nums); New_Line;
      Put("Result: "); Put(First, 0); Put(","); Put(Second, 0); Put("; "); Put(Ops, 0); Put(" ops"); New_Line; -- 5,6
   end Execute;

   --N: Exteded_Index_Type;
begin
   --Put("N:"); Get(N);
   declare
      --Nums: Element_Array(1..N);
      --Target: Element_Type;
      Nums: Element_Array := (10, 13, 2, 7, 15, 11);
      Target: Element_Type := 26;
   begin
      --Get(X);
      --Put("Target:"); Get(Target);
      Execute(Nums, Target);
   end;
end Two_Sum;
