pragma Ada_95;
pragma Suppress (All_Checks);

with Ada.Command_Line;
with Ada.Characters.Latin_1;
with Ada.Text_IO.Text_Streams;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;

use Ada.Command_Line;
use Ada.Characters;
use Ada.Text_IO;
use Ada.Text_IO.Text_Streams;
use Ada.Streams;

procedure Mandelbrot is

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
    procedure Free is new Ada.Unchecked_Deallocation(Stream_Element_Array, Stream_Element_Array_Access);
    Buffer: Stream_Element_Array_Access;

    w, h: Natural := 16000;
    iter: Natural := 50;
    limit: Long_Float := 2.0;

    task type Worker_Type is
        entry Start(P1, P2: Natural);
        entry Wait;
    end Worker_Type;

    task body Worker_Type is
        Y1, Y2: Natural;
        Byte_Num: Stream_Element_Offset := 1;
        Byte: Byte_Type := Null_Byte;
        Bit_Num: Byte_Bit_Num_Type := Byte_Bit_Num_Type'First;
        Bit_Value: Bit_Type;

        kh, kw, limit2: Long_Float;

        Zr, Zi, Cr, Ci, Tr, Ti: Long_Float;
    begin
        accept Start(P1, P2: Natural) do
            Y1 := P1;
            Y2 := P2;
        end Start;
        Put_Line(Standard_Error, "Worker " & Natural'Image(Y1) & ".." & Natural'Image(Y2) & " started");

        kh := 2.0 / Long_Float(h);
        kw := 2.0 / Long_Float(w);
        limit2 := limit * limit;

        for y in Y1..Y2 loop 
            Ci := kh * Long_Float(y) - 1.0;
            for x in 0..w-1 loop
                Cr := kw * Long_Float(x) - 1.5;

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

        Put_Line(Standard_Error, "Worker " & Natural'Image(Y1) & ".." & Natural'Image(Y2) & " stopped");
        accept Wait;
    end Worker_Type;

    type Worker_Array is array(Natural range <>) of Worker_Type;
    type Worker_Array_Access is access Worker_Array;
    procedure Free is new Ada.Unchecked_Deallocation(Worker_Array, Worker_Array_Access);

    Workers: Worker_Array_Access;
    Workers_Num: Natural := 4;

    Y1, Y2, I_Len, Y_Add: Natural;
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
        iter := Natural'Value(Argument(3));
    end if;
    if Argument_Count > 3 then
        limit := Long_Float'Value(Argument(4));
    end if;

    Buffer := new Stream_Element_Array (1..Stream_Element_Offset(w*h/8));

    Workers := new Worker_Array (1..Workers_Num);

    I_Len := h / Workers_Num;
    Y_Add := h rem Workers_Num;
    Y1 := 0;
    for I in Workers'Range loop
        Y2 := Y1 + I_Len - 1;
        if Y_Add /= 0 then
            Y2 := Y2 + 1;
        end if;
        Workers(I).Start(Y1, Y2);
        Y1 := Y2 + 1;
        if Y_Add /= 0 then
            Y_Add := Y_Add - 1;
        end if;
    end loop;
    for I in reverse Workers'Range loop
        Workers(I).Wait;
    end loop;
    Free(Workers);

    String'Write(Stdout, "P4" & Latin_1.CR & Argument(1) & " " & Argument(1) & Latin_1.CR);
    Write(Stdout.all, Buffer.all);
    Free(Buffer);
end Mandelbrot;
