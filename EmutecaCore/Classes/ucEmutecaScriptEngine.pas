unit ucEmutecaScriptEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // Emuteca
  ucEmutecaEmulator, ucEmutecaEmulatorManager,
  ucEmutecaSystem, ucEmutecaSystemManager,
  ucEmutecaParent, ucEmutecaParentManager,
  ucEmutecaSoftware, ucEmutecaSoftManager,
  // CHX
  ucCHXScriptEngine,
  // Imports
  uPSI_ucEmutecaEmulator, uPSI_ucEmutecaEmulatorManager,
  uPSI_ucEmutecaSystem, uPSI_ucEmutecaSystemManager,
  uPSI_ucEmutecaParent, uPSI_ucEmutecaParentManager,
  uPSI_ucEmutecaSoftware, uPSI_ucEmutecaSoftManager;

type

  { cEmutecaScriptEngine }

  cEmutecaScriptEngine = class(cCHXScriptEngine)
  private
    FEmulator: cEmutecaEmulator;
    FEmulatorManager: cEmutecaEmulatorManager;
    FParent: cEmutecaParent;
    FParentManager: cEmutecaParentManager;
    FSoftManager: cEmutecaSoftManager;
    FSoftware: cEmutecaSoftware;
    FSystem: cEmutecaSystem;
    FSystemManager: cEmutecaSystemManager;
    procedure SetEmulator(AValue: cEmutecaEmulator);
    procedure SetEmulatorManager(AValue: cEmutecaEmulatorManager);
    procedure SetParent(AValue: cEmutecaParent);
    procedure SetParentManager(AValue: cEmutecaParentManager);
    procedure SetSoftManager(AValue: cEmutecaSoftManager);
    procedure SetSoftware(AValue: cEmutecaSoftware);
    procedure SetSystem(AValue: cEmutecaSystem);
    procedure SetSystemManager(AValue: cEmutecaSystemManager);

  protected
    procedure PasScriptOnCompImport(Sender: TObject;
      x: TPSPascalCompiler); override;
    procedure PasScriptOnCompile(Sender: TPSScript); override;
    procedure PasScriptOnExecImport(Sender: TObject; se: TPSExec;
      x: TPSRuntimeClassImporter); override;
    procedure PasScriptOnExecute(Sender: TPSScript); override;
    function PasScriptOnFindUnknownFile(Sender: TObject;
      const OrginFileName: tbtstring;
      var FileName, Output: tbtstring): boolean; override;
    function PasScriptOnNeedFile(Sender: TObject;
      const OriginFileName: tbtstring;
      var FileName, Output: tbtstring): boolean; override;

  public
    property Emulator: cEmutecaEmulator read FEmulator write SetEmulator;
    property EmulatorManager: cEmutecaEmulatorManager
      read FEmulatorManager write SetEmulatorManager;
    property System: cEmutecaSystem read FSystem write SetSystem;
    property SystemManager: cEmutecaSystemManager
      read FSystemManager write SetSystemManager;

    property Parent: cEmutecaParent read FParent write SetParent;
    property ParentManager: cEmutecaParentManager
      read FParentManager write SetParentManager;

    property Software: cEmutecaSoftware read FSoftware write SetSoftware;
    property SoftManager: cEmutecaSoftManager
      read FSoftManager write SetSoftManager;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ cEmutecaScriptEngine }

procedure cEmutecaScriptEngine.SetEmulator(AValue: cEmutecaEmulator);
begin
  if FEmulator = AValue then
    Exit;
  FEmulator := AValue;
end;

procedure cEmutecaScriptEngine.SetEmulatorManager(
  AValue: cEmutecaEmulatorManager);
begin
  if FEmulatorManager = AValue then
    Exit;
  FEmulatorManager := AValue;
end;

procedure cEmutecaScriptEngine.SetParent(AValue: cEmutecaParent);
begin
  if FParent = AValue then
    Exit;
  FParent := AValue;
end;

procedure cEmutecaScriptEngine.SetParentManager(AValue:
  cEmutecaParentManager);
begin
  if FParentManager = AValue then
    Exit;
  FParentManager := AValue;
end;

procedure cEmutecaScriptEngine.SetSoftManager(AValue: cEmutecaSoftManager);
begin
  if FSoftManager = AValue then
    Exit;
  FSoftManager := AValue;
end;

procedure cEmutecaScriptEngine.SetSoftware(AValue: cEmutecaSoftware);
begin
  if FSoftware = AValue then
    Exit;
  FSoftware := AValue;
end;

procedure cEmutecaScriptEngine.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;
end;

procedure cEmutecaScriptEngine.SetSystemManager(AValue:
  cEmutecaSystemManager);
begin
  if FSystemManager = AValue then
    Exit;
  FSystemManager := AValue;
end;

procedure cEmutecaScriptEngine.PasScriptOnCompImport(Sender: TObject;
  x: TPSPascalCompiler);
begin
  inherited PasScriptOnCompImport(Sender, x);
end;

procedure cEmutecaScriptEngine.PasScriptOnCompile(Sender: TPSScript);
begin
  inherited PasScriptOnCompile(Sender);
end;

procedure cEmutecaScriptEngine.PasScriptOnExecImport(Sender: TObject;
  se: TPSExec; x: TPSRuntimeClassImporter);
begin
  inherited PasScriptOnExecImport(Sender, se, x);
end;

procedure cEmutecaScriptEngine.PasScriptOnExecute(Sender: TPSScript);
begin
  inherited PasScriptOnExecute(Sender);
end;

function cEmutecaScriptEngine.PasScriptOnFindUnknownFile(Sender: TObject;
  const OrginFileName: tbtstring; var FileName, Output: tbtstring): boolean;
begin
  Result := inherited PasScriptOnFindUnknownFile(Sender,
    OrginFileName, FileName, Output);
end;

function cEmutecaScriptEngine.PasScriptOnNeedFile(Sender: TObject;
  const OriginFileName: tbtstring; var FileName, Output: tbtstring): boolean;
begin
  Result := inherited PasScriptOnNeedFile(Sender, OriginFileName,
    FileName, Output);
end;

constructor cEmutecaScriptEngine.Create;
begin
  inherited;
end;

destructor cEmutecaScriptEngine.Destroy;
begin
  inherited;
end;

end.
