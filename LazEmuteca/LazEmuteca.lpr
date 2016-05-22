program LazEmuteca;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, bs_controls, lz_rtticontrols, pl_pascalscript, pl_virtualtrees,
  ucEmuteca, ufCHXScriptManager, ufSMAskFile, ufSMAskFolder, ucEmutecaParent,
  ucEmutecaConfig, ufEmutecaMain, ucEmutecaFile, ucEmutecaEmulatorManager,
  ucEmutecaParentManager, ucEmutecaSystemManager, ucEmutecaEmulator,
  ufEmutecaVersionList, ucEmutecaVersion, ucEmutecaSystem, 
ufEmutecaEmulatorEditor,
  ufEmutecaEmulatorManager, ufEmutecaSystemManager, ugEmutecaManager,
  ugEmutecaPersList, ufEmutecaScriptManager, ucEmutecaVersionManager, ufTagTree,
  ufEmutecaSystemEditor, ufEmutecaActAddVersion, ufEmutecaVersionEditor, 
uGUIConfig, uCHXvcConfig, uaEmutecaStorable, uaEmutecaManager, uCHXFrmUtils
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmEmutecaMain, frmEmutecaMain);
  Application.Run;
end.

