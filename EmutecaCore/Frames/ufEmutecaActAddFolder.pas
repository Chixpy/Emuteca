unit ufEmutecaActAddFolder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, EditBtn;

type

  { TfmAddFolder }

  TfmAddFolder = class(TFrame)
    bRun: TButton;
    cbxSystem: TComboBox;
    chkIncSubfolders: TCheckBox;
    eFolder: TDirectoryEdit;
    lFolder: TLabel;
    lSystem: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

end.

