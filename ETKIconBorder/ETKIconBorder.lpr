program ETKIconBorder;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ufrIconBorderMain, uCHXStrUtils, uCHXConst, uCHXRscStr, uCHXImageUtils,
  ufrCHXForm, uaCHXConfig, uaCHXStorable, ufCHXFrame, uVersionSupport,
  ucEIBConfig
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title := 'ETK Icon Border';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmIconBorder, frmIconBorder);
  Application.Run;
end.

