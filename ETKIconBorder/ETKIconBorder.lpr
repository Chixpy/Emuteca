program ETKIconBorder;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ufrIconBorderMain, uCHXStrUtils, uCHXConst, uCHXRscStr
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title := 'Emuteca Icon Border';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmIconBorder, frmIconBorder);
  Application.Run;
end.

