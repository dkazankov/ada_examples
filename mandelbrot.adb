pragma Ada_95;
pragma Suppress (All_Checks);

with Ada.Command_Line;
with Ada.Characters.Latin_1;
with Ada.Text_IO.Text_Streams;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;

use Ada.Command_Line;
use Ada.Characters;
use Ada.Text_IO;
use Ada.Text_IO.Text_Streams;
use Ada.Streams;
use Ada.Strings;

procedure Mandelbrot is

    type Real is digits 15;

    type Bit_Type is mod 2;
    for Bit_Type'Size use 1;

    type Byte_Bit_Num_Type is range 0..7;
    type Byte_Bits_Type is array (Byte_Bit_Num_Type) of Bit_Type;
    pragma Pack (Byte_Bits_Type);
    for Byte_Bits_Type'Size use 8;

    type Byte_View is (Byte_As_Stream_Element, Byte_As_Bits);
    type Byte_Type(Option: Byte_View := Byte_View'First) is record
        case Option is
            when Byte_As_Stream_Element =>
                As_Stream_Element: Stream_Element;
            when Byte_As_Bits =>
                As_Bits: Byte_Bits_Type;
        end case;
    end record;
    pragma Unchecked_Union(Byte_Type); -- this is dirty hack: it is Ada 2005 feature, not Ada 95, but GNAT allows it
    for Byte_Type'Size use 8;

    Null_Byte: constant Byte_Type := (Option => Byte_As_Stream_Element, As_Stream_Element => 0);

    type Stream_Element_Array_Access is access Stream_Element_Array;

    w, h: Natural := 16000;
    iter: Natural := 50;
    limit: Real := 2.0;

    procedure Calculate_Chunk(Y1, Y2: Natural; Buffer: Stream_Element_Array_Access) is
        Byte_Num: Stream_Element_Offset := 1;
        Byte: Byte_Type := Null_Byte;
        Bit_Num: Byte_Bit_Num_Type := Byte_Bit_Num_Type'First;
        Bit_Value: Bit_Type;

        kh, kw, limit2: Real;

        Zr, Zi, Cr, Ci, Tr, Ti: Real;
    begin
        kh := 2.0 / Real(h);
        kw := 2.0 / Real(w);
        limit2 := limit * limit;

        for y in Y1..Y2 loop 
            Ci := kh * Real(y) - 1.0;
            for x in 0..w-1 loop
                Cr := kw * Real(x) - 1.5;

                Zr := 0.0;
                Zi := 0.0;

                Bit_Value := 1;
                i_loop: for i in 1..iter loop
                    Tr := Zr*Zr - Zi*Zi + Cr;
                    Ti := 2.0*Zr*Zi + Ci;
                    Zr := Tr;
                    Zi := Ti;
                    if Zr*Zr+Zi*Zi > limit2 then
                        Bit_Value := 0;
                        exit i_loop;
                    end if;
                end loop i_loop;

                if Bit_Value = 1 then
                    Byte.As_Bits(7-Bit_Num) := Bit_Value; -- LE
                end if;
                if Bit_Num = 7 then
                    Buffer(Byte_Num) := Byte.As_Stream_Element;
                    Byte_Num := Byte_Num + 1;
                    Bit_Num := Byte_Bit_Num_Type'First;
                    Byte := Null_Byte;
                elsif x = w-1 then
                    Buffer(Byte_Num) := Byte.As_Stream_Element;
                    Byte_Num := Byte_Num + 1;
                    Bit_Num := Byte_Bit_Num_Type'First;
                    Byte := Null_Byte;
                else
                    Bit_Num := Bit_Num + 1; 
                end if;
            end loop;
        end loop;
    end Calculate_Chunk;

    task type Worker_Type is
        entry Start(Chunk: Natural; Y1, Y2: Natural; Buffer: Stream_Element_Array_Access);
        entry Wait;
    end Worker_Type;

    task body Worker_Type is
        mChunk, mY1, mY2: Natural;
        mBuffer: Stream_Element_Array_Access;
    begin
        accept Start(Chunk: Natural; Y1, Y2: Natural; Buffer: Stream_Element_Array_Access) do
            mChunk := Chunk;
            mY1 := Y1;
            mY2 := Y2;
            mBuffer := Buffer;
        end Start;
        Calculate_Chunk(mY1, mY2, mBuffer);
        accept Wait;
    end Worker_Type;

    function Sum(Value: Natural) return Natural is
        S: Natural := 0;
    begin
        for I in 1..Value loop
            S := S + I;
        end loop;
        return S;
    end Sum;

    Workers_Num: Natural := 32;
    Write_Times: Natural := 5;
    Stdout: Stream_Access := Stream(Standard_Output);
begin
    if Argument_Count > 0 then
        w := Natural'Value(Argument(1));
        w := (w+7)/8*8; -- ensure w and h are multiplies of 8
        h := w;
    end if;
    if Argument_Count > 1 then
        Workers_Num := Natural'Value(Argument(2));
    end if;
    if Argument_Count > 2 then
        Write_Times := Natural'Value(Argument(3));
    end if;
    if Argument_Count > 3 then
        iter := Natural'Value(Argument(4));
    end if;
    if Argument_Count > 4 then
        limit := Real'Value(Argument(5));
    end if;

    String'Write(Stdout, "P4" & Latin_1.CR & Fixed.Trim(Natural'Image(w), Left) & Natural'Image(h) & Latin_1.CR);

    declare
        subtype Worker_Index is Natural range 1..Workers_Num;
        Workers: array (Worker_Index) of Worker_Type;
        Buffer: array (Worker_Index) of Stream_Element_Array_Access;
        Buf_Size: Natural;
        Initial_Chunk_Size: constant Natural := h / Sum(Workers_Num) / Write_Times;
        Chunk_Size_Inc: constant Natural := Initial_Chunk_Size * Write_Times;
        Chunk_Size: Natural := Initial_Chunk_Size;
        Y1, Y2: Natural;
    begin
        Y1 := 0;
        for I in Workers'Range loop
            if I = Workers'Last then
                Y2 := h;
            else
                Y2 := Y1 + Chunk_Size - 1;
            end if;
            Buf_Size := w*(Y2-Y1+1)/8;
            Buffer(I) := new Stream_Element_Array (1..Stream_Element_Offset(Buf_Size));
            Workers(I).Start(I, Y1, Y2, Buffer(I));
            Y1 := Y2 + 1;
            Chunk_Size := Chunk_Size + Chunk_Size_Inc;
        end loop;
        for I in Workers'Range loop
            Workers(I).Wait;
            Write(Stdout.all, Buffer(I).all);
        end loop;
    end;
end Mandelbrot;
