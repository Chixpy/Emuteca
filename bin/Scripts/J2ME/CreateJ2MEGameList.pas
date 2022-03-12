{
[Info]
Creates a Emuteca's Game List from .jar files in a folder.
[Data]
Name=Chixpy
Version=0.01
Date=2020XXXX
[Changes]

[EndInfo]
}
program EmutecaInfo;

type
  TJ2MEForm = class(TForm)
    bSelectFolder: TButton;
  public
    constructor Create(Owner: TComponent); override;
  end;

var
  aForm: TJ2MEForm;

begin
  aForm := TJ2MEForm.Create(nil);

  aForm.ShowModal;
  
  aForm.Free;

  WriteLn('');
  WriteLn('DONE');
  WriteLn('----');
end.
