/****************************************************************************
**
** Copyright (C) 2014 Tim Sander <tim@krieglstein.org>
** Contact: http://www.qt-project.org/legal
**
** This file is part of Qt Creator.
**
** Commercial License Usage
** Licensees holding valid commercial Qt licenses may use this file in
** accordance with the commercial license agreement provided with the
** Software or, alternatively, in accordance with the terms contained in
** a written agreement between you and Digia.  For licensing terms and
** conditions see http://qt.digia.com/licensing.  For further information
** use the contact form at http://qt.digia.com/contact-us.
**
** GNU Lesser General Public License Usage
** Alternatively, this file may be used under the terms of the GNU Lesser
** General Public License version 2.1 as published by the Free Software
** Foundation and appearing in the file LICENSE.LGPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU Lesser General Public License version 2.1 requirements
** will be met: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
**
** In addition, as a special exception, Digia gives you certain additional
** rights.  These rights are described in the Digia Qt LGPL Exception
** version 1.1, included in the file LGPL_EXCEPTION.txt in this package.
**
****************************************************************************/

#include "baremetaldeviceconfigurationwizard.h"
#include "baremetaldeviceconfigurationwizardpages.h"
#include "baremetaldevice.h"
#include "baremetalconstants.h"
#include "ssh/sshconnection.h"

using namespace ProjectExplorer;

namespace BareMetal {
namespace Internal {
namespace {
enum PageId { SetupPageId };
} // anonymous namespace


class BareMetalDeviceConfigrationWizardPrivate
{
public:
    BareMetalDeviceConfigrationWizardPrivate(QWidget *parent):
        m_setupPage(parent)
    { }

    BareMetalDeviceConfigurationWizardSetupPage m_setupPage;
};

} //namespace Internal

BareMetalDeviceConfigurationWizard::BareMetalDeviceConfigurationWizard(QWidget *parent) :
   Utils::Wizard(parent),
   d(new Internal::BareMetalDeviceConfigrationWizardPrivate(this))
{
    setWindowTitle(tr("New Bare Metal Device Configuration Setup"));
    setPage(Internal::SetupPageId, &d->m_setupPage);
    d->m_setupPage.setCommitPage(true);
}

BareMetalDeviceConfigurationWizard::~BareMetalDeviceConfigurationWizard()
{
    delete d;
}

IDevice::Ptr BareMetalDeviceConfigurationWizard::device() const
{
    //sshParams is not really used as ssh parameters but as debugger parameters
    QSsh::SshConnectionParameters sshParams;
    sshParams.host = d->m_setupPage.gdbHostname();
    sshParams.port = d->m_setupPage.gdbPort();
    Internal::BareMetalDevice::Ptr device = Internal::BareMetalDevice::create(d->m_setupPage.configurationName(),
                                                  Core::Id(Constants::BareMetalOsType),
                                                  IDevice::Hardware);
    device->setSshParameters(sshParams);
    device->setGdbInitCommands(d->m_setupPage.gdbInitCommands());
    return device;
}

} //namespace BareMetal
