unit uPSI_ucEmuteca;

interface

uses
  SysUtils, Classes, uPSComponent, uPSRuntime, uPSCompiler,
  uEmutecaCommon,
  ucEmuteca, ucEmutecaConfig, ucEmutecaEmulatorManager,
  ucEmutecaSystemManager;


{ compile-time registration functions }
procedure SIRegister_ucEmuteca(CL: TPSPascalCompiler);
// includes:
procedure SIRegister_cEmuteca(CL: TPSPascalCompiler);


{ run-time registration functions }
procedure RIRegister_ucEmuteca(CL: TPSRuntimeClassImporter);
// includes:
procedure RIRegister_cEmuteca(CL: TPSRuntimeClassImporter);


implementation

procedure SIRegister_cEmuteca(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TComponent'), 'cEmuteca') do
  begin
    RegisterProperty('ProgressCallBack', 'TEmutecaProgressCallBack', iptrw);
    RegisterProperty('TempFolder', 'string', iptr);
    //    RegisterMethod('Procedure LoadConfig(aFile : string)');
    //    RegisterMethod('Procedure ClearAllData');
    //    RegisterMethod('Procedure LoadData');
    RegisterMethod('Procedure SaveData');
    //    RegisterMethod('Procedure CacheData');
    RegisterMethod('Function RunSoftware(const aSoftware: cEmutecaSoftware): integer');
    RegisterProperty('BaseFolder', 'string', iptrw);
    RegisterProperty('Config', 'cEmutecaConfig', iptr);
    RegisterProperty('SystemManager', 'cEmutecaSystemManager', iptr);
    RegisterProperty('EmulatorManager', 'cEmutecaEmulatorManager', iptr);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_ucEmuteca(CL: TPSPascalCompiler);
begin
  SIRegister_cEmuteca(CL);
end;

(* === run-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure cEmutecaEmulatorManager_R(Self: cEmuteca;
  var T: cEmutecaEmulatorManager);
begin
  T := Self.EmulatorManager;
end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemManager_R(Self: cEmuteca;
  var T: cEmutecaSystemManager);
begin
  T := Self.SystemManager;
end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaConfig_R(Self: cEmuteca; var T: cEmutecaConfig);
begin
  T := Self.Config;
end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaBaseFolder_W(Self: cEmuteca; const T: string);
begin
  Self.BaseFolder := T;
end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaBaseFolder_R(Self: cEmuteca; var T: string);
begin
  T := Self.BaseFolder;
end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaTempFolder_R(Self: cEmuteca; var T: string);
begin
  T := Self.TempFolder;
end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaProgressCallBack_W(Self: cEmuteca;
  const T: TEmutecaProgressCallBack);
begin
  Self.ProgressCallBack := T;
end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaProgressCallBack_R(Self: cEmuteca;
  var T: TEmutecaProgressCallBack);
begin
  T := Self.ProgressCallBack;
end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaCacheDataThreadTempFolder_W(Self: cEmutecaCacheDataThread;
  const T: string);
begin
  Self.TempFolder := T;
end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaCacheDataThreadTempFolder_R(Self: cEmutecaCacheDataThread;
  var T: string);
begin
  T := Self.TempFolder;
end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaCacheDataThreadSystemManager_W(Self: cEmutecaCacheDataThread;
  const T: cEmutecaSystemManager);
begin
  Self.SystemManager := T;
end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaCacheDataThreadSystemManager_R(Self: cEmutecaCacheDataThread;
  var T: cEmutecaSystemManager);
begin
  T := Self.SystemManager;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_cEmuteca(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(cEmuteca) do
  begin
    RegisterPropertyHelper(@cEmutecaProgressCallBack_R,
      @cEmutecaProgressCallBack_W, 'ProgressCallBack');
    RegisterPropertyHelper(@cEmutecaTempFolder_R, nil, 'TempFolder');
   // RegisterMethod(@cEmuteca.LoadConfig, 'LoadConfig');
   // RegisterMethod(@cEmuteca.ClearAllData, 'ClearAllData');
   // RegisterMethod(@cEmuteca.LoadData, 'LoadData');
    RegisterMethod(@cEmuteca.SaveData, 'SaveData');
   // RegisterMethod(@cEmuteca.CacheData, 'CacheData');
    RegisterMethod(@cEmuteca.RunSoftware, 'RunSoftware');
    RegisterPropertyHelper(@cEmutecaBaseFolder_R, @cEmutecaBaseFolder_W,
      'BaseFolder');
    RegisterPropertyHelper(@cEmutecaConfig_R, nil, 'Config');
    RegisterPropertyHelper(@cEmutecaSystemManager_R, nil, 'SystemManager');
    RegisterPropertyHelper(@cEmutecaEmulatorManager_R, nil, 'EmulatorManager');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_cEmutecaCacheDataThread(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(cEmutecaCacheDataThread) do
  begin
    RegisterPropertyHelper(@cEmutecaCacheDataThreadSystemManager_R,
      @cEmutecaCacheDataThreadSystemManager_W, 'SystemManager');
    RegisterPropertyHelper(@cEmutecaCacheDataThreadTempFolder_R,
      @cEmutecaCacheDataThreadTempFolder_W, 'TempFolder');
    RegisterConstructor(@cEmutecaCacheDataThread.Create, 'Create');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_ucEmuteca(CL: TPSRuntimeClassImporter);
begin
  RIRegister_cEmutecaCacheDataThread(CL);
  RIRegister_cEmuteca(CL);
end;

end.
