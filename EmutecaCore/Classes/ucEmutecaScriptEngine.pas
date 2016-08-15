unit ucEmutecaScriptEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // Pascal Script main units
  uPSComponent, uPSRuntime, uPSCompiler, uPSUtils,
  // Emuteca
  ucEmuteca, ucEmutecaConfig,
  ucEmutecaEmulator, ucEmutecaEmulatorManager,
  ucEmutecaSystem, ucEmutecaSystemManager,
  ucEmutecaParent, ucEmutecaParentManager,
  ucEmutecaSoftware, ucEmutecaSoftManager,
  // CHX
  ucCHXScriptEngine,
  // Imports
  uPSI_ucEmuteca, uPSI_ucEmutecaConfig,
  uPSI_ucEmutecaEmulator, uPSI_ucEmutecaEmulatorManager,
  uPSI_ucEmutecaSystem, uPSI_ucEmutecaSystemManager,
  uPSI_ucEmutecaParent, uPSI_ucEmutecaParentManager,
  uPSI_ucEmutecaSoftware, uPSI_ucEmutecaSoftManager;

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

  { TODO : Check if correct order }
  SIRegister_cEmutecaConfig(x);
  SIRegister_ucEmutecaConfig(x);
  SIRegister_cEmutecaSoftManager(x);
  SIRegister_ucEmutecaSoftManager(x);
  SIRegister_cEmutecaVersion(x);
  SIRegister_ucEmutecaSoftware(x);
  SIRegister_cEmutecaParentManager(x);
  SIRegister_ucEmutecaParentManager(x);
  SIRegister_cEmutecaParent(x);
  SIRegister_ucEmutecaParent(x);
  SIRegister_cEmutecaSystemManager(x);
  SIRegister_ucEmutecaSystemManager(x);
  SIRegister_cEmutecaSystem(x);
  SIRegister_ucEmutecaSystem(x);
  SIRegister_cEmutecaEmulatorManager(x);
  SIRegister_ucEmutecaEmulatorManager(x);
  SIRegister_cEmutecaEmulator(x);
  SIRegister_ucEmutecaEmulator(x);
  SIRegister_cEmuteca(x);
  SIRegister_ucEmuteca(x);
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

procedure cEmutecaScriptEngine.PasScriptOnExecImport(Sender: TObject;
  se: TPSExec; x: TPSRuntimeClassImporter);
begin
  inherited PasScriptOnExecImport(Sender, se, x);

  { TODO : Check correct order }
  RIRegister_cEmutecaConfig(x);
  RIRegister_ucEmutecaConfig(x);
  RIRegister_cEmutecaSoftManager(x);
  RIRegister_ucEmutecaSoftManager(x);
  RIRegister_cEmutecaVersion(x);
  RIRegister_ucEmutecaSoftware(x);
  RIRegister_cEmutecaParentManager(x);
  RIRegister_ucEmutecaParentManager(x);
  RIRegister_cEmutecaParent(x);
  RIRegister_ucEmutecaParent(x);
  RIRegister_cEmutecaSystemManager(x);
  RIRegister_ucEmutecaSystemManager(x);
  RIRegister_ucEmutecaSystem_Routines(se);
  RIRegister_cEmutecaSystem(x);
  RIRegister_ucEmutecaSystem(x);
  RIRegister_cEmutecaEmulatorManager(x);
  RIRegister_ucEmutecaEmulatorManager(x);
  RIRegister_cEmutecaEmulator(x);
  RIRegister_ucEmutecaEmulator(x);
  RIRegister_cEmuteca(x);
  RIRegister_ucEmuteca(x);
end;

end.
