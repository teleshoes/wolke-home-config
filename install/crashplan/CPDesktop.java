package com.backup42.desktop;

import com.backup42.common.AuthorityLocation;
import com.backup42.common.CPVersion;
import com.backup42.common.OrgType;
import com.backup42.common.User;
import com.backup42.common.config.CentralConfig;
import com.backup42.common.config.OrgTypeConfigItem;
import com.backup42.common.config.ServiceConfig;
import com.backup42.common.config.ServicePeerConfig;
import com.backup42.common.config.ServiceUIConfig;
import com.backup42.common.perm.C42PermissionPro.CPD;
import com.backup42.common.perm.C42PermissionPro.CPS;
import com.backup42.desktop.actions.ShowMainWindow;
import com.backup42.desktop.controllers.MainWindowController;
import com.backup42.desktop.events.Publisher;
import com.backup42.desktop.events.service.ConnectFailedEvent;
import com.backup42.desktop.events.service.ConnectedEvent;
import com.backup42.desktop.interfaces.IModelObserver;
import com.backup42.desktop.layout.CPFormBuilder;
import com.backup42.desktop.layout.CPGridFormBuilder;
import com.backup42.desktop.layout.CPMigFormBuilder;
import com.backup42.desktop.model.AppModel;
import com.backup42.desktop.model.ConfigModel;
import com.backup42.desktop.model.LicenseModel;
import com.backup42.desktop.model.SystemModel;
import com.backup42.desktop.model.UserListModel;
import com.backup42.desktop.model.UserModel;
import com.backup42.desktop.utils.CPFont;
import com.backup42.desktop.utils.DesktopProperties;
import com.backup42.desktop.view.MainWindow;
import com.backup42.desktop.view.MainWindow.Event.AppActivatedEvent;
import com.backup42.desktop.view.MainWindow.Event.AppCloseEvent;
import com.backup42.desktop.view.MainWindow.Event.AppDeactivatedEvent;
import com.backup42.desktop.view.MainWindow.Event.AppShowEvent;
import com.backup42.desktop.view.MainWindow.Event.Listener;
import com.backup42.service.AuthorityLocationUtil;
import com.backup42.service.CPText;
import com.backup42.service.CpsFolders;
import com.backup42.service.ui.UIInfoUtility;
import com.backup42.service.ui.UIInfoUtility.UIConnectionDetailsResult;
import com.backup42.service.ui.message.StatusResponseMessage;
import com.backup42.service.ui.message.UpdateLicenseMessage;
import com.code42.auth.ILicense;
import com.code42.backup.C42PermissionBackup.Backup;
import com.code42.config.ConfigItem;
import com.code42.crypto.StringHasher;
import com.code42.event.IListener;
import com.code42.event.Listener;
import com.code42.i18n.LocaleUtil;
import com.code42.i18n.Text;
import com.code42.io.FileUtility;
import com.code42.lang.ClassPathHacker;
import com.code42.lang.NativeLibraryLoader;
import com.code42.logging.C42LoggerConfig;
import com.code42.logging.Logger;
import com.code42.logging.LoggerFactory;
import com.code42.messaging.Location;
import com.code42.os.mac.io.FileManager;
import com.code42.os.mac.io.FileManager.FSCatalogInfo;
import com.code42.os.posix.PosixProcessCommands;
import com.code42.os.win.process.ProcessUtil;
import com.code42.perm.C42PermissionBase.Admin;
import com.code42.perm.C42PermissionBase.None;
import com.code42.perm.PermissionUtils;
import com.code42.swt.layout.FormBuilder;
import com.code42.swt.layout.GridFormBuilder;
import com.code42.swt.layout.MigFormBuilder;
import com.code42.swt.util.ActionManager;
import com.code42.swt.util.SWTExec;
import com.code42.swt.util.SWTPathUtil;
import com.code42.swt.view.AppWindowEvent.WindowReadyEvent;
import com.code42.utils.AppUtil;
import com.code42.utils.ArrayUtils;
import com.code42.utils.Formatter;
import com.code42.utils.LangUtils;
import com.code42.utils.Os;
import com.code42.utils.PropertiesUtil;
import com.code42.utils.Stopwatch;
import com.code42.utils.SystemProperties;
import com.code42.watcher.SystemWatcher;
import com.code42.xml.XmlTool;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Locale;
import java.util.Properties;
import org.apache.logging.log4j.Level;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

public class CPDesktop
  extends Publisher
  implements SplashWindow.Event.Listener, MainWindow.Event.Listener, IModelObserver
{
  private static final Logger log = LoggerFactory.getLogger(CPDesktop.class);
  private static final Object MAIN_MONITOR = new Object[0];
  private static Display display;
  private SplashWindow splashWindow;
  private final Listener listener = new Listener(this);
  private Services services;
  private final Object[] connectMonitor = new Object[0];
  private final AppModel appModel;
  private static String appBaseName;
  private PropertiesUtil customProps;
  
  public static void main(String[] args)
    throws Throwable
  {
    try
    {
      String tmpdirname = System.getProperty("java.io.tmpdir");
      if (tmpdirname == null)
      {
        log.info("No JVM temp directory configured!", new Object[0]);
        System.exit(1);
      }
      File tmpdir = new File(tmpdirname);
      if (!tmpdir.exists())
      {
        log.info("JVM temp directory " + tmpdirname + " does not exist!", new Object[0]);
        System.exit(1);
      }
      if (!tmpdir.isDirectory())
      {
        log.info("JVM temp directory " + tmpdirname + " is not a directory!", new Object[0]);
        System.exit(1);
      }
      if (!tmpdir.canWrite())
      {
        log.info("JVM temp directory " + tmpdirname + " exists but we can't write to it!", new Object[0]);
        System.exit(1);
      }
      File swtDir = new File(tmpdirname + System.getProperty("file.separator") + ".cpswt");
      
      SystemProperties.setProperty("swt.library.path", swtDir.getAbsolutePath());
      String jvm = SystemProperties.getJvmDetails();
      String os = SystemProperties.getOsDetails();
      String user = System.getProperty("user.name") + ", " + System.getProperty("user.home");
      
      log.info("*************************************************************", new Object[0]);
      log.info("*************************************************************", new Object[0]);
      log.info("STARTED " + getAppBaseName() + "Desktop", new Object[0]);
      log.info("CPVERSION = " + CPVersion.asString(), new Object[0]);
      log.info("ARGS      = " + ArrayUtils.toString(args), new Object[0]);
      log.info("LOCALE    = " + Locale.getDefault().getDisplayName(Locale.ENGLISH), new Object[0]);
      log.info("JVM       = " + jvm, new Object[0]);
      log.info("OS        = " + os, new Object[0]);
      log.info("User      = " + user, new Object[0]);
      log.info("swt.library.path = " + System.getProperty("swt.library.path"), new Object[0]);
      log.info("*************************************************************", new Object[0]);
      if (swtDir.exists())
      {
        File[] swtFiles = swtDir.listFiles();
        if (swtFiles != null) {
          for (File swtFile : swtFiles)
          {
            boolean deleted = swtFile.delete();
            if (deleted) {
              log.info("SWT library deleted: " + swtFile, new Object[0]);
            }
          }
        }
      }
      else
      {
        boolean swtDirCreated = swtDir.mkdir();
        if (swtDirCreated) {
          log.info("SWT library dir created: " + swtDir, new Object[0]);
        }
      }
      SWTPathUtil.addSwtToPath();
      AppTimer.begin(CPDesktop.class.getSimpleName());
      System.setProperty("file.encoding", "UTF-8");
      XmlTool.useInternalDocumentBuilderFactory();
      
      CPDesktop startupController = new CPDesktop(args);
      startupController.launch();
    }
    catch (Throwable e)
    {
      String msg = "Failed to launch " + CPDesktop.class.getSimpleName() + "; " + e;
      log.error(msg, new Object[] { e });
      throw e;
    }
  }
  
  public static void secondaryMain(String[] args)
  {
    log.info("Bring main window forward.", new Object[0]);
    Display disp = MainWindow.getInstance().getShell().getDisplay();
    ActionManager.run(disp, new ShowMainWindow());
  }
  
  public CPDesktop(String[] args)
    throws Exception
  {
    Properties commandLineArguments = AppUtil.getCommandLineProperties(args);
    
    AppTimer.begin("PreSplash");
    
    String waitValue = commandLineArguments.getProperty("waitForCustom");
    if (Boolean.valueOf(waitValue).booleanValue()) {
      waitForCustom();
    }
    String devConfigName = commandLineArguments.getProperty("devConfigName");
    if (SystemProperties.isDevEnv())
    {
      if (!LangUtils.hasValue(devConfigName)) {
        throw new Exception("Unable to start, missing devConfigName property.");
      }
      String appDataPath = SystemProperties.getOptional("c42.app.commonDataFolder");
      String skinPath = "skin";
      if (LangUtils.hasValue(devConfigName))
      {
        ServiceConfig sc = new ServiceConfig();
        String xml = FileUtility.readTextFile("../app_service/conf/test/" + devConfigName + ".xml");
        sc.fromXml(xml);
        OrgType orgType = orgType.get();
        if (orgType.equals(OrgType.BUSINESS)) {
          skinPath = "skin_blue";
        } else if (orgType.equals(OrgType.ENTERPRISE)) {
          skinPath = "skin_black";
        }
      }
      File skinFolder = new File(skinPath);
      log.info("Skin folder: {}", new Object[] { skinFolder.getAbsolutePath() });
      ClassPathHacker.addFile(skinFolder);
      if (LangUtils.hasValue(appDataPath))
      {
        appDataPath = appDataPath + "/" + devConfigName;
        String absoluteAppDataPath = new File(appDataPath).getAbsolutePath();
        SystemProperties.setProperty("c42.app.commonDataFolder", absoluteAppDataPath);
      }
    }
    customProps = new PropertiesUtil(new Properties());
    customProps.load(new File(CpsFolders.getCustomParent(appBaseName), "conf/custom.properties"), false);
    
    LangUtils.registerImpl(FormBuilder.class, CPFormBuilder.class);
    LangUtils.registerImpl(GridFormBuilder.class, CPGridFormBuilder.class);
    LangUtils.registerImpl(MigFormBuilder.class, CPMigFormBuilder.class);
    Text.setInstance(CPText.getTextInstance());
    Text.getInstance().setOverride(customProps.getProperties());
    
    String appName = CPText.getAppName();
    Display.setAppName(appName);
    
    appModel = new AppModel(commandLineArguments);
    appModel.getConfigModel().addObserver(this);
    if (customProps.getOptionalBoolean("ssoAuth.enabled", false))
    {
      boolean required = customProps.getOptionalBoolean("ssoAuth.required", false);
      String provider = customProps.getOptional("ssoAuth.provider", "single sign-on");
      
      appModel.setupSso(required, provider);
    }
    String liblog = loadNativeLibraries();
    CpdFolders.init();
    CpdFoldersDeprecated.moveConfigFile();
    appModel.getDesktopProperties().loadMyProperties(CpdFolders.getConfFile());
    
    waitForUiInfo();
    
    UIInfoUtility.UIConnectionDetailsResult connectionDetails = UIInfoUtility.getUIConnectionDetails();
    
    String localeString = appModel.getDesktopProperties().getLocale();
    Locale localle = setDefaultLocale(localeString);
    Text.getInstance().setUseNames(DesktopProperties.getInstance().isTextShowNames());
    
    String jvm = SystemProperties.getJvmDetails();
    String os = SystemProperties.getOsDetails();
    String user = System.getProperty("user.name") + ", " + System.getProperty("user.home");
    
    readLoggingConfiguration();
    log.info("*************************************************************", new Object[0]);
    log.info("*************************************************************", new Object[0]);
    log.info("STARTED " + getAppBaseName() + "Desktop - " + connectionDetails.getAddress() + ":" + connectionDetails.getPort(), new Object[0]);
    log.info("CPVERSION = " + CPVersion.asString(), new Object[0]);
    log.info("BUILD     = 1", new Object[0]);
    log.info("ARGS      = " + ArrayUtils.toString(args), new Object[0]);
    log.info("LOCALE    = " + localle.getDisplayName(Locale.ENGLISH), new Object[0]);
    log.info("JVM       = " + jvm, new Object[0]);
    log.info("OS        = " + os, new Object[0]);
    log.info("User      = " + user, new Object[0]);
    log.info(liblog, new Object[0]);
    ClassPathHacker.logURLs(Level.INFO);
    
    appModel.getDesktopProperties().log();
    SystemProperties.dumpProperties(log.getName(), Level.INFO);
    log.info("*************************************************************", new Object[0]);
    SystemProperties.logMemory("", Level.INFO);
    
    testNativeLibraries();
    XmlTool.log();
    
    display = Display.getDefault();
    
    CPFont.loadFonts(display, appModel.getDesktopProperties());
    
    StringHasher.C42.verifyMethodsExist(log);
    
    AppTimer.end("PreSplash");
    log.info("*************************************************************", new Object[0]);
  }
  
  public static String getAppBaseName()
  {
    if (appBaseName == null) {
      appBaseName = SystemProperties.getOptional("appBaseName", "CrashPlan");
    }
    return appBaseName;
  }
  
  private void waitForCustom()
  {
    File customMark = new File(CpsFolders.getCustomParent(appBaseName), "~custom");
    log.info("Waiting for custom indicator to appear in " + customMark, new Object[0]);
    try
    {
      Stopwatch sw = new Stopwatch();
      long maxWait = 10000L;
      while ((!customMark.exists()) && (sw.getElapsed() < 10000L)) {
        Thread.sleep(250L);
      }
      log.info("Waited " + Formatter.getDurationString(sw.getElapsed()) + " for custom indicator to appear in " + customMark + ", exists=" + customMark.exists(), new Object[0]);
    }
    catch (InterruptedException e)
    {
      log.info("InterruptedException while waiting for custom indicator to appear in " + customMark, new Object[0]);
    }
  }
  
  private void waitForUiInfo()
    throws FileNotFoundException
  {
    File uiInfo = new File(UIInfoUtility.FILEPATH);
    log.info("Waiting for .ui_info file to appear in " + uiInfo, new Object[0]);
    try
    {
      Stopwatch sw = new Stopwatch();
      long maxWait = 10000L;
      while ((!uiInfo.exists()) && (sw.getElapsed() < 10000L)) {
        Thread.sleep(250L);
      }
      log.info("Waited " + Formatter.getDurationString(sw.getElapsed()) + " for .ui_info to appear in " + uiInfo + ", exists=" + uiInfo.exists(), new Object[0]);
    }
    catch (InterruptedException e)
    {
      log.info("InterruptedException while waiting for .ui_info to appear in " + uiInfo, new Object[0]);
    }
    if (!uiInfo.exists()) {
      throw new FileNotFoundException(".ui_info file not found");
    }
  }
  
  private String loadNativeLibraries()
  {
    String libLog = null;
    try
    {
      libLog = NativeLibraryLoader.appendToPath();
    }
    catch (Exception e)
    {
      log.warn("Unable to load native libraries, exiting!", new Object[] { e });
      System.exit(1);
    }
    return libLog;
  }
  
  private void testNativeLibraries()
  {
    AppTimer.begin("testNativeLibraries");
    if ((SystemProperties.isOs(Os.Macintosh)) || (SystemProperties.isOs(Os.Linux)))
    {
      log.info("uid: " + PosixProcessCommands.getuid(), new Object[0]);
      log.info("gid: " + PosixProcessCommands.getgid(), new Object[0]);
      log.info("pid: " + PosixProcessCommands.getpid(), new Object[0]);
    }
    if (SystemProperties.isOs(Os.Macintosh))
    {
      File testFile = new File(".");
      FileManager fm = FileManager.getInstance();
      FileManager.FSCatalogInfo info = fm.getCatalogInfo(testFile);
      log.info("info: " + info, new Object[0]);
    }
    if (SystemProperties.isOs(Os.Windows)) {
      log.info("pid: " + ProcessUtil.getProcessID(), new Object[0]);
    }
    AppTimer.end("testNativeLibraries");
  }
  
  private void launch()
    throws Exception
  {
    try
    {
      splashWindow = new SplashWindow(display);
      splashWindow.addListeners(new IListener[] { listener });
      splashWindow.open();
    }
    catch (Throwable e)
    {
      log.warn("Unable to show splash. " + e.getMessage(), new Object[] { e });
    }
    SystemWatcher.setCheckDelay(1000L);
    
    UIInfoUtility.UIConnectionDetailsResult connectionDetails = UIInfoUtility.getUIConnectionDetails();
    services = new Services(connectionDetails.getAddress(), connectionDetails.getPort().intValue());
    
    services.addListener(listener, ConnectedEvent.class);
    services.addListener(listener, ConnectFailedEvent.class);
    services.addListener(listener, StatusResponseMessage.class);
    
    Stopwatch sw = new Stopwatch();
    
    PermissionUtils.init(new Class[] { C42PermissionBackup.Backup.class, C42PermissionBase.Admin.class, C42PermissionBase.None.class, C42PermissionPro.CPD.class, C42PermissionPro.CPS.class });
    
    log.info("Loaded permissions in " + sw, new Object[0]);
    
    log.info("Adding shutdown hook.", new Object[0]);
    Runtime.getRuntime().addShutdownHook(new Thread()
    {
      public void run()
      {
        try
        {
          CPDesktop.log.debug("ShutdownHook...calling cleanup", new Object[0]);
          
          CPDesktop.log.info("EXITING... Normally", new Object[0]);
          services.stop();
          SystemWatcher.stop();
          CPDesktop.log.debug("ShutdownHook...sleeping 3 seconds", new Object[0]);
          Thread.sleep(1000L);
          synchronized (CPDesktop.MAIN_MONITOR)
          {
            CPDesktop.log.debug("ShutdownHook...notify main of shutdown.", new Object[0]);
            CPDesktop.MAIN_MONITOR.notifyAll();
          }
        }
        catch (InterruptedException e) {}
        CPDesktop.log.debug("ShutdownHook...calling halt.", new Object[0]);
        
        Runtime.getRuntime().halt(0);
      }
    });
    connect(false);
    try
    {
      while (!display.isDisposed()) {
        try
        {
          if (!display.readAndDispatch()) {
            display.sleep();
          }
        }
        catch (Throwable e)
        {
          log.warn(e.toString(), new Object[] { e });
          display.sleep();
        }
      }
    }
    finally
    {
      SWTExec.shutdown();
      System.exit(0);
    }
  }
  
  private void connect(final boolean retry)
  {
    Thread thread = new Thread("connect")
    {
      public void run()
      {
        if (retry)
        {
          CPDesktop.log.info("Restarting service...", new Object[0]);
          splashWindow.showConnectingMessage();
        }
        UIInfoUtility.UIConnectionDetailsResult connectionDetails = UIInfoUtility.getUIConnectionDetails();
        String host = connectionDetails.getAddress();
        int port = connectionDetails.getPort().intValue();
        long delay = appModel.getDesktopProperties().getConnectRetryDelay();
        long retryAttempts = appModel.getDesktopProperties().getConnectRetryAttempts();
        long attempts = retryAttempts + 1L;
        CPDesktop.log.info("Connecting to service at " + new Location(host, port), new Object[0]);
        synchronized (connectMonitor)
        {
          connectionWait(700L);
          for (int i = 1; i <= attempts; i++)
          {
            if (i > 1)
            {
              CPDesktop.log.info("    FAILED on attempt #" + (i - 1) + ", retrying in " + delay + "ms", new Object[0]);
              connectionWait(delay);
            }
            try
            {
              services.connect();
              
              connectionWait(30000L);
            }
            catch (IOException e)
            {
              CPDesktop.log.warn("Unable to establish connection.", new Object[] { e });
              break;
            }
            if (services.isConnected())
            {
              CPDesktop.log.info("    SUCCESS on attempt #" + i, new Object[0]);
              break;
            }
            if (services.isConnecting())
            {
              CPDesktop.log.warn("    FAILED on attempt #" + i + ", aborting because something is seriously wrong.", new Object[0]);
              break;
            }
            if (i == attempts) {
              CPDesktop.log.info("    FAILED on attempt #" + i + ", done", new Object[0]);
            }
          }
          if (!services.isConnected()) {
            splashWindow.showConnectFailedMessage();
          }
        }
      }
      
      private void connectionWait(long delay)
      {
        try
        {
          connectMonitor.wait(delay);
        }
        catch (InterruptedException ie)
        {
          Thread.interrupted();
          CPDesktop.log.error("Interrupted while waiting to connect!", new Object[0]);
        }
      }
    };
    thread.setDaemon(true);
    thread.start();
  }
  
  public void handleEvent(ConnectFailedEvent event)
  {
    synchronized (connectMonitor)
    {
      connectMonitor.notifyAll();
    }
  }
  
  public void handleEvent(ConnectedEvent event)
  {
    log.info("  Connected to service", new Object[0]);
    services.getApplicationStatus();
    synchronized (connectMonitor)
    {
      connectMonitor.notifyAll();
    }
  }
  
  public void handleModelUpdate(ConfigModel model)
  {
    String locale = appModel.getDesktopProperties().getLocale();
    String localeConfig = (String)getConfigserviceUI.locale.getValue();
    if (!LangUtils.equals(locale, localeConfig)) {
      appModel.getDesktopProperties().setLocale(localeConfig);
    }
  }
  
  private Locale setDefaultLocale(String confLocale)
  {
    if (LangUtils.hasValue(confLocale))
    {
      Locale locale = LocaleUtil.getLocale(confLocale);
      if (locale != null)
      {
        String msg = "Set Language Locale to " + confLocale;
        log.info(msg, new Object[0]);
        Locale.setDefault(locale);
        CPText.clearInstance();
        Text.setInstance(CPText.getTextInstance());
        if (AppModel.getInstance().getOrgType() != null) {
          CPText.setOrgType(AppModel.getInstance().getOrgType());
        }
        Text.getInstance().setOverride(customProps.getProperties());
        Text.getInstance().setUseNames(DesktopProperties.getInstance().isTextShowNames());
        return locale;
      }
    }
    return Locale.getDefault();
  }
  
  public void handleEvent(StatusResponseMessage event)
  {
    log.info("  Initial application state received", new Object[0]);
    UpdateLicenseMessage lm = event.getLicenseMessage();
    ILicense lic = lm.getLicense();
    boolean authenticated = lm.isAuthenticated();
    appModel.setFirstInstall(!authenticated);
    appModel.setReauthorizeError(event.getReauthorizeError());
    appModel.getLicenseModel().setLicense(lic, authenticated, event.getUser().getUsername(), lm.getPassword(), lm.getAuthorizeRules(), lm.isBlocked());
    
    appModel.getSystem().setSecurityKey(lm.getSecurityKeyType(), lm.isDataKeyExists(), lm.isSecureDataKeyExists(), lm.getSecureDataKeyQA(), lm.getCustomKey());
    
    appModel.getSystem().setServiceOs(event.getOs());
    appModel.getSystem().setMacRootVolumeName(event.getMacRootVolumeName());
    appModel.setOrgType(event.getOrgType());
    SystemModel.getInstance().setDefaultRestorePath(event.getDefaultRestoreFolder());
    if (event.getUser() != null)
    {
      appModel.getUsers().setMyUser(event.getUser());
      
      String username = appModel.getUsers().getMyUser().getUsername();
      if (AuthorityLocation.isMissingParam(username))
      {
        String usernameProp = SystemProperties.getOptional("user.name");
        username = AuthorityLocationUtil.parseString(username, usernameProp);
        appModel.getUsers().getMyUser().getUserObject().setUsername(username);
      }
    }
    String currentLocale = appModel.getDesktopProperties().getLocale();
    if (!LangUtils.equals(currentLocale, event.getLocale()))
    {
      appModel.getDesktopProperties().setLocale(event.getLocale());
      setDefaultLocale(event.getLocale());
    }
    if (LangUtils.hasValue(event.getPassword()))
    {
      appModel.getConfigModel().getConfig().serviceUI.autoLogin.setValue(Boolean.valueOf(true));
      appModel.getConfigModel().getConfig().serviceUI.autoLoginPasswordHash.setValue(event.getPassword());
    }
    appModel.getConfigModel().getConfig().servicePeer.authority.setValue(event.getAuthority());
    if (LangUtils.hasValue(event.getWebsiteHost())) {
      appModel.getConfigModel().getConfig().servicePeer.centralConfig.websiteHost.setValue(event.getWebsiteHost());
    }
    if (event.isUpgrading()) {
      splashWindow.showUpgrading();
    } else {
      createMainWindow();
    }
  }
  
  private void createMainWindow()
  {
    display.asyncExec(new Runnable()
    {
      public void run()
      {
        CPDesktop self = CPDesktop.this;
        MainWindow mainWindow = new MainWindow(CPDesktop.display, appModel, services);
        mainWindow.addListener(listener, new Class[] { MainWindow.Event.AppCloseEvent.class, MainWindow.Event.AppShowEvent.class, AppWindowEvent.WindowReadyEvent.class });
        
        new MainWindowController(mainWindow, appModel, services);
      }
    });
  }
  
  public void handleEvent(AppWindowEvent.WindowReadyEvent event)
  {
    if (!splashWindow.isDisposed()) {
      splashWindow.close();
    }
  }
  
  private void readLoggingConfiguration()
  {
    log.trace("BackupService", new Object[] { "readLoggingConfiguration" });
    
    String logfile = CpdFolders.getLogFile();
    log.info("Log file: " + logfile, new Object[0]);
    System.setProperty("c42.log.file", logfile);
    
    String configFile = SystemProperties.getOptional("c42.log.config", "conf/ui.log.xml");
    C42LoggerConfig.reconfigure(configFile);
  }
  
  public void handleEvent(SplashWindow.Event.RetryEvent event)
  {
    connect(true);
  }
  
  public void handleEvent(MainWindow.Event.AppShowEvent event)
  {
    services.removeAlert("");
  }
  
  public void handleEvent(MainWindow.Event.AppCloseEvent event) {}
  
  public void handleEvent(MainWindow.Event.AppActivatedEvent event) {}
  
  public void handleEvent(MainWindow.Event.AppDeactivatedEvent event) {}
  
  public static void setupTestApp()
  {
    LangUtils.registerImpl(FormBuilder.class, CPFormBuilder.class);
    LangUtils.registerImpl(GridFormBuilder.class, CPGridFormBuilder.class);
    LangUtils.registerImpl(MigFormBuilder.class, CPMigFormBuilder.class);
    Text.setInstance(CPText.getTextInstance());
    new AppModel();
    CPFont.loadFonts(Display.getDefault(), new Properties());
  }
  
  public static void close()
  {
    if (!display.isDisposed()) {
      display.close();
    }
  }
}

/* Location:
 * Qualified Name:     com.backup42.desktop.CPDesktop
 * Java Class Version: 7 (51.0)
 * JD-Core Version:    0.7.1
 */