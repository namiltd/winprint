program WinPrint;

uses
  Forms,
  Windows,

  MyStrings in 'source\auxiliary\MyStrings.pas', {MyStrings}
  NumEdit in 'source\auxiliary\NumEdit.pas', {NumEdit}
  TrayIcon in 'source\auxiliary\TrayIcon.pas', {TrayIcon}
  CEVersionInfo in 'source\auxiliary\CEVersionInfo.pas', {CEVersionInfo}
  SetString in 'source\auxiliary\SetString.pas', {SetString}

  MainFormUnit in 'source\MainFormUnit.pas' {MainForm},
  ConfigFormUnit in 'source\ConfigFormUnit.pas' {ConfigForm},
  PrintStringsUnit in 'source\PrintStringsUnit.pas',
  ConversionUnit in 'source\ConversionUnit.pas';

{$R *.RES}

begin
  Application.Initialize;
  SetWindowLong(Application.Handle, GWL_EXSTYLE, WS_EX_TOOLWINDOW); //prevents flickering
  Application.ShowMainForm:=false;
  Application.Title:='';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TConfigForm, ConfigForm);
  Application.Run;
end.
