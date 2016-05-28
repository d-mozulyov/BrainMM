unit Unit1;

{
   Copyright (c) Steve Maughan:
   http://www.stevemaughan.com/delphi/delphi-parallel-programming-library-memory-managers/

   Modyfied by Dmitry Mozulyov: https://github.com/d-mozulyov/BrainMM
   Binaries: http://dmozulyov.ucoz.net/BrainMM/Demo.rar
}

interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uSpeedTest, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  SpeedTest: TSpeedTest;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Time, P: Integer;
  S: string;
begin
  SpeedTest := TSpeedTest.Create(50000);
  SpeedTest.Multicore := CheckBox1.Checked;

  Button1.Enabled := False;
  Screen.Cursor := crHourGlass;
  Time := SpeedTest.Execute;

  SpeedTest.Free;
  Button1.Enabled := True;
  Screen.Cursor := crDefault;

  S := ChangeFileExt(ExtractFileName(Application.ExeName), '');
  P := Pos('.', S);
  if (P <> 0) then Delete(S, 1, P);
  Form1.Caption := Format('%s time: %dms', [S, Time]);
end;

end.
