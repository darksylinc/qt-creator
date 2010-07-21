/**************************************************************************
**
** This file is part of Qt Creator
**
** Copyright (c) 2010 Nokia Corporation and/or its subsidiary(-ies).
**
** Contact: Nokia Corporation (qt-info@nokia.com)
**
** Commercial Usage
**
** Licensees holding valid Qt Commercial licenses may use this file in
** accordance with the Qt Commercial License Agreement provided with the
** Software or, alternatively, in accordance with the terms contained in
** a written agreement between you and Nokia.
**
** GNU Lesser General Public License Usage
**
** Alternatively, this file may be used under the terms of the GNU Lesser
** General Public License version 2.1 as published by the Free Software
** Foundation and appearing in the file LICENSE.LGPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU Lesser General Public License version 2.1 requirements
** will be met: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
**
** If you are unsure which license is appropriate for your use, please
** contact the sales department at http://qt.nokia.com/contact.
**
**************************************************************************/

#ifndef QMLJSDELTA_H
#define QMLJSDELTA_H

#include "qmljsprivateapi.h"

#include <qmljs/qmljsdocument.h>
#include <qmljs/parser/qmljsastvisitor_p.h>
#include <qmljs/parser/qmljsastfwd_p.h>
#include <qmljs/parser/qmljsast_p.h>

using namespace QmlJS;
using namespace QmlJS::AST;

namespace QmlJSInspector {
namespace Internal {

class Delta
{
public:
    QSet<UiObjectMember *> newObjects;

    typedef QHash< UiObjectMember*, QList<QDeclarativeDebugObjectReference > > DebugIdMap;
    DebugIdMap operator()(const QmlJS::Document::Ptr &doc1, const QmlJS::Document::Ptr &doc2, const DebugIdMap &debugIds);


    QmlJS::Document::Ptr document() const;
    QmlJS::Document::Ptr previousDocument() const;

private:
    void insert(UiObjectMember *member, UiObjectMember *parentMember,
                const QList<QDeclarativeDebugObjectReference> &debugReferences, const Document::Ptr &doc);
    void update(UiObjectDefinition* oldObject, const QmlJS::Document::Ptr& oldDoc,
                UiObjectDefinition* newObject, const QmlJS::Document::Ptr& newDoc,
                const QList<QDeclarativeDebugObjectReference >& debugReferences);
    void remove(const QList< QDeclarativeDebugObjectReference > &debugReferences);

protected:
    virtual void updateScriptBinding(const QDeclarativeDebugObjectReference &objectReference,
                             QmlJS::AST::UiScriptBinding *scriptBinding,
                             const QString &propertyName,
                             const QString &scriptCode);
    virtual void updateMethodBody(const QDeclarativeDebugObjectReference &objectReference,
                            UiScriptBinding *scriptBinding,
                            const QString &methodName,
                            const QString &methodBody);
    virtual void resetBindingForObject(int debugId, const QString &propertyName);
    virtual void removeObject(int debugId);
    virtual void createObject(const QString &qmlText, const QDeclarativeDebugObjectReference &ref,
                         const QStringList &importList, const QString &filename);

private:
    QmlJS::Document::Ptr m_currentDoc;
    QmlJS::Document::Ptr m_previousDoc;
};

} // namespace Internal
} // namespace QmlJSInspector

#endif // QMLJSDELTA_H

