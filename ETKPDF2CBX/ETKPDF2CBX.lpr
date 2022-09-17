program ETKPDF2CBX;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ufrMain, ufrCHXForm, uVersionSupport, uCHXConst, uCHXStrUtils,
  uCHXRscStr, uCHXFileUtils, uCHXImageUtils, uCHXExecute, uCHX7zWrapper,
  uaCHXConfig, uaCHXStorable, ufCHXFrame, ufCHXImgListPreview,
  ufCHXFileListPreview, ufCHXImgViewer, ufCHXListPreview, uP2CConst, ufP2CMain,
  ucP2CConfig
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrMain, frMain);
  Application.Run;
end.

