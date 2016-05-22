unit ugEmutecaManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl,
  uEmutecaCommon,
  ugEmutecaPersList;

{ TODO : Implement Generic manager for emulators, systems, parents and software
    as they use the same interface...
  }
  (*  type

  { cEmutecaGenManager }

  generic cEmutecaGenManager<T> = class(TComponent)
  private

  protected

  public
  //  property List: TFPGObjectList read FList;
  //  property CurrentList: TFPGObjectList read FCurrentList;

    property ProgressCallBack: TEmutecaProgressCallBack;
    //< CallBack function to show the progress in actions.

    procedure LoadFromFile;
    // Loads current Items file
    procedure SaveToFile;
    // Saves current Items file

    procedure ImportFile(const aFileName: string);
    // Imports data to the current list
    procedure ExportFile(const aFileName: string);
    // Exports data of the current list

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property DataFile: string;

  end;

*)
implementation


end.

