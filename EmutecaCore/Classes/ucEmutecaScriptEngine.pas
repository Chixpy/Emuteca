unit ucEmutecaScriptEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // Pascal Script main units
  uPSComponent, uPSRuntime, uPSCompiler, uPSUtils,
  // Emuteca
  ucEmuteca,
  // CHX
  ucCHXScriptEngine,
  // PS Imports
  uPSI_uEmutecaCommon,
  //uPSI_uaCHXStorable, uPSI_uaEmutecaManager,
  uPSI_ucEmutecaEmulator, uPSI_ucEmutecaSystem,
  uPSI_ucEmutecaGroup, uPSI_ucEmutecaSoftware,
  uPSI_ucEmutecaEmulatorManager, uPSI_ucEmutecaSystemManager,
  uPSI_ucEmutecaGroupManager, uPSI_ucEmutecaSoftManager,
  uPSI_ucEmutecaConfig, uPSI_ucEmuteca;

type

  { cEmutecaScriptEngine }

  cEmutecaScriptEngine = class(cCHXScriptEngine)
  private
    FEmuteca: cEmuteca;
    procedure SetEmuteca(AValue: cEmuteca);

  protected
    procedure PasScriptOnCompImport(Sender: TObject;
      x: TPSPascalCompiler); override;
    procedure PasScriptOnCompile(Sender: TPSScript); override;
    procedure PasScriptOnExecImport(Sender: TObject; se: TPSExec;
      x: TPSRuntimeClassImporter); override;
    procedure PasScriptOnExecute(Sender: TPSScript); override;
    function PasScriptOnFindUnknownFile(Sender: TObject;
      const OriginFileName: tbtstring;
      var FileName, Output: tbtstring): boolean; override;
    function PasScriptOnNeedFile(Sender: TObject;
      const OriginFileName: tbtstring;
      var FileName, Output: tbtstring): boolean; override;


  public
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ cEmutecaScriptEngine }

procedure cEmutecaScriptEngine.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;
end;

procedure cEmutecaScriptEngine.PasScriptOnCompImport(Sender: TObject;
  x: TPSPascalCompiler);
begin
  inherited PasScriptOnCompImport(Sender, x);

  SIRegister_uEmutecaCommon(x);

  //SIRegister_uaCHXStorable(x);
  //SIRegister_uaEmutecaManager(x);
  SIRegister_ucEmutecaEmulator(x);
  SIRegister_ucEmutecaSystem(x);
  SIRegister_ucEmutecaGroup(x);
  SIRegister_ucEmutecaSoftware(x);
  SIRegister_ucEmutecaEmulatorManager(x);
  SIRegister_ucEmutecaSystemManager(x);
  SIRegister_ucEmutecaGroupManager(x);
  SIRegister_ucEmutecaSoftManager(x);
  SIRegister_ucEmutecaConfig(x);
  SIRegister_ucEmuteca(x);
end;

procedure cEmutecaScriptEngine.PasScriptOnExecImport(Sender: TObject;
  se: TPSExec; x: TPSRuntimeClassImporter);
begin
  inherited PasScriptOnExecImport(Sender, se, x);

  RIRegister_uEmutecaCommon_Routines(se);

  //RIRegister_uaCHXStorable(x);
  //RIRegister_uaEmutecaManager(x);
  RIRegister_ucEmutecaEmulator(x);
  //RIRegister_ucEmutecaSystem_Routines(se);
  RIRegister_ucEmutecaSystem(x);
  RIRegister_ucEmutecaGroup(x);
  RIRegister_ucEmutecaSoftware(x);
  RIRegister_ucEmutecaEmulatorManager(x);
  RIRegister_ucEmutecaSystemManager(x);
  RIRegister_ucEmutecaGroupManager(x);
  RIRegister_ucEmutecaSoftManager(x);
  RIRegister_ucEmutecaConfig(x);
  RIRegister_ucEmuteca(x);
end;

procedure cEmutecaScriptEngine.PasScriptOnCompile(Sender: TPSScript);
begin
  inherited PasScriptOnCompile(Sender);

  // Variables
  Sender.AddRegisteredPTRVariable('Emuteca', 'cEmuteca');
end;

procedure cEmutecaScriptEngine.PasScriptOnExecute(Sender: TPSScript);
begin
  inherited PasScriptOnExecute(Sender);

  Sender.SetPointerToData('Emuteca', @FEmuteca,
    Sender.FindNamedType('cEmuteca'));
end;

function cEmutecaScriptEngine.PasScriptOnFindUnknownFile(Sender: TObject;
  const OriginFileName: tbtstring; var FileName, Output: tbtstring): boolean;
begin
  Result := inherited PasScriptOnFindUnknownFile(Sender,
    OriginFileName, FileName, Output);
end;

function cEmutecaScriptEngine.PasScriptOnNeedFile(Sender: TObject;
  const OriginFileName: tbtstring; var FileName, Output: tbtstring): boolean;
begin
  Result := inherited PasScriptOnNeedFile(Sender, OriginFileName,
    FileName, Output);
end;

constructor cEmutecaScriptEngine.Create;
begin
  inherited Create;
end;

destructor cEmutecaScriptEngine.Destroy;
begin
  inherited Destroy;
end;

end.
