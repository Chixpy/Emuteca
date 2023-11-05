program ETKDBEditor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ufrETKDBEditorMain, ufrCHXForm, ufCHXFrame, uCHXImageUtils,
  uCHXStrUtils, uCHXRscStr, uCHXConst, uEmutecaRscStr,
  uEmutecaConst, ufETKDBEditorMain, uETKDBEConst, uETKDBERscStr
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmETKDBEditorMain, frmETKDBEditorMain);
  Application.Run;
end.

