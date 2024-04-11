with Ada.Text_IO;             use Ada.Text_IO;
with STM32.GPIO;              use STM32.GPIO;
with STM32.Board;             use STM32.Board;
with STM32.Device;            use STM32.Device;
with HAL.Bitmap;              use HAL.Bitmap;
with BMP_Fonts;
with Bitmapped_Drawing;
with Bitmap_Color_Conversion; use Bitmap_Color_Conversion;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Lab3 is

    package Str renames Ada.Strings.Unbounded;

    Col1 : GPIO_Point := PC9;
    Col2 : GPIO_Point := PE4;
    Col3 : GPIO_Point := PE5;
    Col4 : GPIO_Point := PE6;

    Row1_state : Boolean := False;
    Row2_state : Boolean := False;
    Row3_state : Boolean := False;
    Row4_state : Boolean := False;

    Col1_state : Boolean := False;
    Col2_state : Boolean := False;
    Col3_state : Boolean := False;
    Col4_state : Boolean := False;

    Row1 : GPIO_Point := PC8;
    Row2 : GPIO_Point := PA8;
    Row3 : GPIO_Point := PG9;
    Row4 : GPIO_Point := PA9;

    Col_Pins : array (1 .. 4) of GPIO_Point := (Col1, Col2, Col3, Col4);
    Row_Pins : array (1 .. 4) of GPIO_Point := (Row1, Row2, Row3, Row4);

    type Key_State is (Pressed, Released);
    type Key is record
       S1 : Key_State;
       S2 : Key_State;
       C : Character;
    end record;

    type Keys_Array is array (Row_Pins'Range, Col_Pins'Range) of Key;

    Keys : Keys_Array := ((Key'(Released, '1'), Key'(Released, '2'), Key'(Released, '3'),Key'(Released, 'A')),
                          (Key'(Released, '4'), Key'(Released, '5'), Key'(Released, '6'),Key'(Released, 'B')),
                          (Key'(Released, '7'), Key'(Released, '8'), Key'(Released, '9'),Key'(Released, 'C')),
                          (Key'(Released, '*'), Key'(Released, '0'), Key'(Released, '#'),Key'(Released, 'D')));
    -- 1 2 3 a
    -- 4 5 6 b
    -- 7 8 9 c
    -- * 0 # d




    Input_String : String (1 .. 10) := (others => ' '); -- max number
    Input_Length : Natural          := 0; -- current length

    state : Boolean := False;


    procedure High (This : in out GPIO_Point) renames STM32.GPIO.Clear;
    procedure Low (This : in out GPIO_Point) renames STM32.GPIO.Set;

    procedure Configure_Analog_Input (This : in out GPIO_Point) is
    begin
        Enable_Clock (This);
        Configure_IO (This, (Mode => Mode_In, Resistors => Floating));
    end Configure_Analog_Input;

    procedure Configure_Analog_Output (This : in out GPIO_Point) is
    begin
        Enable_Clock (This);
        Configure_IO
           (This,
            (Mode => Mode_Out, Output_Type => Push_Pull, Speed => Speed_100MHz,
             Resistors => Floating));
    end Configure_Analog_Output;

    procedure Print_To_LCD (Str : String) is
        BG : constant Bitmap_Color := (Alpha => 255, others => 64);
        FG : constant Bitmap_Color := (Alpha => 255, others => 255);
    begin
        Display.Hidden_Buffer (1).Set_Source (BG);
        Display.Hidden_Buffer (1).Fill;

        Bitmapped_Drawing.Draw_String
           (Display.Hidden_Buffer (1).all, Start => (0, 0), Msg => Str,
            Font => BMP_Fonts.Font16x24, Foreground => FG, Background => BG);

        Display.Update_Layer (1, Copy_Back => True);
    end Print_To_LCD;


    function "+" (S:Str.Unbounded_String) return String is
    begin
        return Str.To_String(S);
    end;
    function "+" (S: String) return Unbounded_String is
    begin
        return Str.To_Unbounded_String(S);
    end;

    Output : Str.Unbounded_String := +"";

    procedure Update_Key (Row_Idx : Integer; Col_Idx : Integer) is
    begin
        if not Set (Row_Pins (Row_Idx)) then
            Keys (Row_Idx, Col_Idx).S := Pressed;
        end if;
    end;

    procedure Validate_Key (Row_Idx : Integer; Col_Idx : Integer) is
        KS : Key_State := Keys (Row_Idx, Col_Idx).S;
    begin
        if not Set (Row_Pins (Row_Idx)) and KS = Pressed then
            Str.Append (Output, Keys (Row_Idx, Col_Idx).C);
        end if;
    end;

begin
    Configure_Analog_Input (Row1);
    Configure_Analog_Input (Row2);
    Configure_Analog_Input (Row3);
    Configure_Analog_Input (Row4);
    Configure_Analog_Output (Col1);
    Configure_Analog_Output (Col2);
    Configure_Analog_Output (Col3);
    Configure_Analog_Output (Col4);
    Low (Col1);
    Low (Col2);
    Low (Col3);
    Low (Col4);
    Display.Initialize;
    Display.Initialize_Layer (1, ARGB_8888);
    loop
        for Col_Index in Col_Pins'Range loop
            High (Col_Pins (Col_Index));
            for Row_Index in Row_Pins'Range loop
                Update_Key (Row_Index, Col_Index);
            end loop;
            delay 0.05;
            for Row_Index in Row_Pins'Range loop
                Validate_Key (Row_Index, Col_Index);
            end loop;
            Low (Col_Pins (Col_Index));
        end loop;
        Print_To_LCD(+Output);
        Keys := (others => (others => (S => Released)));
    end loop;
    --  if not Set(Row1) then
    --      if not Row1_state then
    --          -- pressed
    --          if Input_Length < Input_String'Length then
    --              Input_Length := Input_Length + 1;
    --              Input_String(Input_Length) := '1';
    --              Print_To_LCD(Input_String(1 .. Input_Length));
    --          end if;
    --          Row1_state := True;
    --      end if;
    --  else
    --      Row1_state := False;  -- reset
    --  end if;
    --  if not Set(Row2) then
    --      if not Row2_state then
    --          -- pressed
    --          if Input_Length < Input_String'Length then
    --              Input_Length := Input_Length + 1;
    --              Input_String(Input_Length) := '4';
    --              Print_To_LCD(Input_String(1 .. Input_Length));
    --          end if;
    --          Row2_state := True;
    --      end if;
    --  else
    --      Row2_state := False;  -- reset
    --  end if;
    --  if not Set(Row3) then
    --      if not Row3_state then
    --          -- pressed
    --          if Input_Length < Input_String'Length then
    --              Input_Length := Input_Length + 1;
    --              Input_String(Input_Length) := '7';
    --              Print_To_LCD(Input_String(1 .. Input_Length));
    --          end if;
    --          Row3_state := True;
    --      end if;
    --  else
    --      Row3_state := False;  -- reset
    --  end if;
    --  if not Set(Row4) then
    --      if not Row4_state then
    --          -- pressed
    --          if Input_Length < Input_String'Length then
    --              Input_Length := Input_Length + 1;
    --              Input_String(Input_Length) := '*';
    --              Print_To_LCD(Input_String(1 .. Input_Length));
    --          end if;
    --          Row4_state := True;  --
    --      end if;
    --  else
    --      Row4_state := False;  -- reset
    --  end if;

end Lab3;