program LazEmuteca;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, bs_controls, lz_rtticontrols, pl_pascalscript, pl_virtualtrees,
  ucEmuteca, uPSI_ucEmuteca, ucEmutecaParent, ucEmutecaConfig, ufEmutecaMain,
  ucEmutecaFile, ucEmutecaEmulatorManager, ucEmutecaParentManager,
  ucEmutecaSystemManager, ucEmutecaEmulator, ufEmutecaSoftList,
  ucEmutecaSoftware, ucEmutecaSystem, ufEmutecaEmulatorEditor,
  ufEmutecaEmulatorManager, ufEmutecaSystemManager, ugEmutecaManager,
  ugEmutecaPersList, ufEmutecaScriptManager, ucEmutecaSoftManager,
  ufEmutecaSystemEditor, ufEmutecaActAddSoft, ufEmutecaSoftEditor, uGUIConfig,
  uaEmutecaStorable, uaEmutecaManager, ufEmutecaSystemInfoEditor,
  ufEmutecaActAddFolder, ufEmutecaSystemEditorExt, ucEmutecaScriptEngine,
  ufESMSoftList, ufEmutecaSystemCBX, ufESMParentList, ufEmutecaChkSoftList,
  ucEmutecaSystemExtra, ufEmutecaParentCBX;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmEmutecaMain, frmEmutecaMain);
  Application.Run;
end.

