program WinPrint;

{$R 'resources\icons32x32\icons.res' 'resources\icons32x32\icons.rc'}

uses
  Forms,
  Windows,

  //Misc Components sources
  MyStrings in 'source\auxiliary\MyStrings.pas',
  NumEdit in 'source\auxiliary\NumEdit.pas',
  TrayIcon in 'source\auxiliary\TrayIcon.pas',
  CEVersionInfo in 'source\auxiliary\CEVersionInfo.pas',
  SetString in 'source\auxiliary\SetString.pas',
  Xml10n in 'source\auxiliary\Xml10n\source\Xml10n.pas',

  //Xml10n sources
  QRTTI in 'source\auxiliary\Xml10n\source\QRTTI.pas',
  QXmlRTTI in 'source\auxiliary\Xml10n\source\QXmlRTTI.pas',
  QXml in 'source\auxiliary\Xml10n\source\QXml.pas',
  XmlRL in 'source\auxiliary\Xml10n\source\XmlRL.pas',
  XmlUtils in 'source\auxiliary\Xml10n\source\XmlUtils.pas',
  XmlRS in 'source\auxiliary\Xml10n\source\XmlRS.pas',
  Xml10nLnks in 'source\auxiliary\Xml10n\source\Xml10nLnks.pas',
  Xml10nVcl in 'source\auxiliary\Xml10n\source\Xml10nVcl.pas',

  //WinPrint sources
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
