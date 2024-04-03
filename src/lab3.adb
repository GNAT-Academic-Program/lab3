with Ada.Text_IO;  use Ada.Text_IO;
with STM32.GPIO;   use STM32.GPIO;
with STM32.Board;  use STM32.Board;
with STM32.Device; use STM32.Device;
with HAL.Bitmap;  use HAL.Bitmap;
with BMP_Fonts;
with Bitmapped_Drawing;
with Bitmap_Color_Conversion; use Bitmap_Color_Conversion;

procedure Lab3 is
    Col1  : GPIO_Point := PC3;
    Row1_state : Boolean := False;
    Row1  : GPIO_Point := PG6;
    Row2  : GPIO_Point := PG7;
    Row3  : GPIO_Point := PG8;
    Row4  : GPIO_Point := PG9;

    state : Boolean    := False;

    procedure High (This : in out GPIO_Point) renames STM32.GPIO.Clear;
    procedure Low (This : in out GPIO_Point) renames STM32.GPIO.Set;

    procedure Configure_Analog_Input(This : in out GPIO_Point) is
    begin
        Enable_Clock (This);
        Configure_IO
           (This,
            (Mode => Mode_In, Resistors => Floating));
    end Configure_Analog_Input;

    procedure Configure_Analog_Output(This : in out GPIO_Point) is
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
begin
Configure_Analog_Input(Row1);
Configure_Analog_Input(Row2);
Configure_Analog_Input(Row3);
Configure_Analog_Input(Row4);
Configure_Analog_Output(Col1);
High (Col1);
Display.Initialize;
Display.Initialize_Layer (1, ARGB_8888);
    loop
        if Set(Col1) then
            Print_To_LCD("1");
        end if;
        --  if state then
        --      High (Col1);
        --      state := False;
        --  else
        --      Low (Col1);
        --      state := True;
        --  end if;
        
    end loop;
end Lab3;