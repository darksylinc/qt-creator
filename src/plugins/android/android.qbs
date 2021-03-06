import qbs.base 1.0

import QtcPlugin

QtcPlugin {
    name: "Android"

    Depends { name: "Core" }
    Depends { name: "ProjectExplorer" }
    Depends { name: "QmakeProjectManager" }
    Depends { name: "Debugger" }
    Depends { name: "QmlDebug" }
    Depends { name: "QtSupport" }
    Depends { name: "TextEditor" }
    Depends { name: "AnalyzerBase" }
    Depends { name: "Utils" }
    Depends { name: "Qt"; submodules: ["widgets", "xml", "network"] }

    property bool enable: false
    pluginspecreplacements: ({"ANDROID_EXPERIMENTAL_STR": (enable ? "false": "true")})

    files: [
        "addnewavddialog.ui",
        "android.qrc",
        "androidanalyzesupport.cpp",
        "androidanalyzesupport.h",
        "androidconfigurations.cpp",
        "androidconfigurations.h",
        "androidconstants.h",
        "androidcreatekeystorecertificate.cpp",
        "androidcreatekeystorecertificate.h",
        "androidcreatekeystorecertificate.ui",
        "androiddeployqtstep.cpp",
        "androiddeployqtstep.h",
        "androiddebugsupport.cpp",
        "androiddebugsupport.h",
        "androiddevicedialog.cpp",
        "androiddevicedialog.h",
        "androiddevicedialog.ui",
        "androiddeployconfiguration.cpp",
        "androiddeployconfiguration.h",
        "androiddeploystep.cpp",
        "androiddeploystep.h",
        "androiddeploystepfactory.cpp",
        "androiddeploystepfactory.h",
        "androiddeploystepwidget.cpp",
        "androiddeploystepwidget.h",
        "androiddeploystepwidget.ui",
        "androiddeployqtwidget.cpp",
        "androiddeployqtwidget.h",
        "androiddeployqtwidget.ui",
        "androiddevice.cpp",
        "androiddevice.h",
        "androiddevicefactory.cpp",
        "androiddevicefactory.h",
        "androiderrormessage.h",
        "androiderrormessage.cpp",
        "androidextralibrarylistmodel.cpp",
        "androidextralibrarylistmodel.h",
        "androidgdbserverkitinformation.cpp",
        "androidgdbserverkitinformation.h",
        "androidglobal.h",
        "androidmanager.cpp",
        "androidmanager.h",
        "androidmanifestdocument.cpp",
        "androidmanifestdocument.h",
        "androidmanifesteditor.cpp",
        "androidmanifesteditor.h",
        "androidmanifesteditorfactory.cpp",
        "androidmanifesteditorfactory.h",
        "androidmanifesteditorwidget.cpp",
        "androidmanifesteditorwidget.h",
        "androidpackagecreationfactory.cpp",
        "androidpackagecreationfactory.h",
        "androidpackagecreationstep.cpp",
        "androidpackagecreationstep.h",
        "androidpackagecreationwidget.cpp",
        "androidpackagecreationwidget.h",
        "androidpackagecreationwidget.ui",
        "androidpackageinstallationfactory.cpp",
        "androidpackageinstallationfactory.h",
        "androidpackageinstallationstep.cpp",
        "androidpackageinstallationstep.h",
        "androidplugin.cpp",
        "androidplugin.h",
        "androidpotentialkit.cpp",
        "androidpotentialkit.h",
        "androidqtversion.cpp",
        "androidqtversion.h",
        "androidqtversionfactory.cpp",
        "androidqtversionfactory.h",
        "androidrunconfiguration.cpp",
        "androidrunconfiguration.h",
        "androidruncontrol.cpp",
        "androidruncontrol.h",
        "androidrunfactories.cpp",
        "androidrunfactories.h",
        "androidrunner.cpp",
        "androidrunner.h",
        "androidrunsupport.cpp",
        "androidrunsupport.h",
        "androidsettingspage.cpp",
        "androidsettingspage.h",
        "androidsettingswidget.cpp",
        "androidsettingswidget.h",
        "androidsettingswidget.ui",
        "androidsignaloperation.cpp",
        "androidsignaloperation.h",
        "androidtoolchain.cpp",
        "androidtoolchain.h",
        "certificatesmodel.cpp",
        "certificatesmodel.h",
        "createandroidmanifestwizard.cpp",
        "createandroidmanifestwizard.h",
        "javaautocompleter.cpp",
        "javaautocompleter.h",
        "javacompletionassistprovider.cpp",
        "javacompletionassistprovider.h",
        "javaeditor.cpp",
        "javaeditor.h",
        "javaeditorfactory.cpp",
        "javaeditorfactory.h",
        "javafilewizard.cpp",
        "javafilewizard.h",
        "javaindenter.cpp",
        "javaindenter.h",
        "javaparser.cpp",
        "javaparser.h",
    ]
}
