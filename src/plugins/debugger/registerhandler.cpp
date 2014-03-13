/****************************************************************************
**
** Copyright (C) 2014 Digia Plc and/or its subsidiary(-ies).
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

#include "registerhandler.h"
#include "watchdelegatewidgets.h"

#if USE_REGISTER_MODEL_TEST
#include <modeltest.h>
#endif

#include <QScrollBar>
#include <QTextEdit>

#include <utils/qtcassert.h>

namespace Debugger {
namespace Internal {

//////////////////////////////////////////////////////////////////
//
// Register
//
//////////////////////////////////////////////////////////////////

enum RegisterType
{
    RegisterUnknown,
    //RegisterDummy,  // like AH if EAX is present.
    RegisterI8,
    RegisterI16,
    RegisterI32,
    RegisterI64,
    RegisterI128,
    RegisterF32,
    RegisterF64,
    RegisterF80,
    RegisterXMM,
    RegisterMMX,
    RegisterNeon,
    RegisterFlags32
};

static struct RegisterNameAndType
{
    const char *name;
    RegisterType type;
} theNameAndType[] = {
    // ARM
    { "r0", RegisterI32 },
    { "r1", RegisterI32 },
    { "r2", RegisterI32 },
    { "r3", RegisterI32 },
    { "r4", RegisterI32 },
    { "r5", RegisterI32 },
    { "r6", RegisterI32 },
    { "r7", RegisterI32 },
    { "r8", RegisterI32 },
    { "r9", RegisterI32 },
    { "r10", RegisterI32 },
    { "r11", RegisterI32 },
    { "r12", RegisterI32 },
    { "sp", RegisterI32 },
    { "lr", RegisterI32 },
    { "pc", RegisterI32 },
    { "cpsr", RegisterFlags32 },
    { "d0", RegisterI64 },
    { "d1", RegisterI64 },
    { "d2", RegisterI64 },
    { "d3", RegisterI64 },
    { "d4", RegisterI64 },
    { "d5", RegisterI64 },
    { "d6", RegisterI64 },
    { "d7", RegisterI64 },
    { "d8", RegisterI64 },
    { "d9", RegisterI64 },
    { "d10", RegisterI64 },
    { "d11", RegisterI64 },
    { "d12", RegisterI64 },
    { "d13", RegisterI64 },
    { "d14", RegisterI64 },
    { "d15", RegisterI64 },
    { "d16", RegisterI64 },
    { "d17", RegisterI64 },
    { "d18", RegisterI64 },
    { "d19", RegisterI64 },
    { "d20", RegisterI64 },
    { "d21", RegisterI64 },
    { "d22", RegisterI64 },
    { "d23", RegisterI64 },
    { "d24", RegisterI64 },
    { "d25", RegisterI64 },
    { "d26", RegisterI64 },
    { "d27", RegisterI64 },
    { "d28", RegisterI64 },
    { "d29", RegisterI64 },
    { "d30", RegisterI64 },
    { "d31", RegisterI64 },
    { "fpscr", RegisterFlags32 },
    { "s0", RegisterI32 },
    { "s1", RegisterI32 },
    { "s2", RegisterI32 },
    { "s3", RegisterI32 },
    { "s4", RegisterI32 },
    { "s5", RegisterI32 },
    { "s6", RegisterI32 },
    { "s7", RegisterI32 },
    { "s8", RegisterI32 },
    { "s9", RegisterI32 },
    { "s10", RegisterI32 },
    { "s11", RegisterI32 },
    { "s12", RegisterI32 },
    { "s13", RegisterI32 },
    { "s14", RegisterI32 },
    { "s15", RegisterI32 },
    { "s16", RegisterI32 },
    { "s17", RegisterI32 },
    { "s18", RegisterI32 },
    { "s19", RegisterI32 },
    { "s20", RegisterI32 },
    { "s21", RegisterI32 },
    { "s22", RegisterI32 },
    { "s23", RegisterI32 },
    { "s24", RegisterI32 },
    { "s25", RegisterI32 },
    { "s26", RegisterI32 },
    { "s27", RegisterI32 },
    { "s28", RegisterI32 },
    { "s29", RegisterI32 },
    { "s30", RegisterI32 },
    { "s31", RegisterI32 },
    { "q0", RegisterI128 },
    { "q1", RegisterI128 },
    { "q2", RegisterI128 },
    { "q3", RegisterI128 },
    { "q4", RegisterI128 },
    { "q5", RegisterI128 },
    { "q6", RegisterI128 },
    { "q7", RegisterI128 },
    { "q8", RegisterI128 },
    { "q9", RegisterI128 },
    { "q10", RegisterI128 },
    { "q11", RegisterI128 },
    { "q12", RegisterI128 },
    { "q13", RegisterI128 },
    { "q14", RegisterI128 },
    { "q15", RegisterI128 },

    // Intel
    { "eax", RegisterI32 },
    { "ecx", RegisterI32 },
    { "edx", RegisterI32 },
    { "ebx", RegisterI32 },
    { "esp", RegisterI32 },
    { "ebp", RegisterI32 },
    { "esi", RegisterI32 },
    { "edi", RegisterI32 },
    { "eip", RegisterI32 },
    { "eflags", RegisterFlags32 },
    { "cs", RegisterI32 },
    { "ss", RegisterI32 },
    { "ds", RegisterI32 },
    { "es", RegisterI32 },
    { "fs", RegisterI32 },
    { "gs", RegisterI32 },
    { "st0", RegisterF80 },
    { "st1", RegisterF80 },
    { "st2", RegisterF80 },
    { "st3", RegisterF80 },
    { "st4", RegisterF80 },
    { "st5", RegisterF80 },
    { "st6", RegisterF80 },
    { "st7", RegisterF80 },
    { "fctrl", RegisterFlags32 },
    { "fstat", RegisterFlags32 },
    { "ftag", RegisterFlags32 },
    { "fiseg", RegisterFlags32 },
    { "fioff", RegisterFlags32 },
    { "foseg", RegisterFlags32 },
    { "fooff", RegisterFlags32 },
    { "fop", RegisterFlags32 },
    { "xmm0", RegisterXMM },
    { "xmm1", RegisterXMM },
    { "xmm2", RegisterXMM },
    { "xmm3", RegisterXMM },
    { "xmm4", RegisterXMM },
    { "xmm5", RegisterXMM },
    { "xmm6", RegisterXMM },
    { "xmm7", RegisterXMM },
    { "xmm8", RegisterXMM },
    { "xmm9", RegisterXMM },
    { "xmm10", RegisterXMM },
    { "xmm11", RegisterXMM },
    { "xmm12", RegisterXMM },
    { "xmm13", RegisterXMM },
    { "xmm14", RegisterXMM },
    { "xmm15", RegisterXMM },
    { "mxcsr", RegisterFlags32 },
    { "orig_eax", RegisterI32 },
    { "al", RegisterI8 },
    { "cl", RegisterI8 },
    { "dl", RegisterI8 },
    { "bl", RegisterI8 },
    { "ah", RegisterI8 },
    { "ch", RegisterI8 },
    { "dh", RegisterI8 },
    { "bh", RegisterI8 },
    { "ax", RegisterI16 },
    { "cx", RegisterI16 },
    { "dx", RegisterI16 },
    { "bx", RegisterI16 },
    { "bp", RegisterI16 },
    { "si", RegisterI16 },
    { "di", RegisterI16 },
    { "mm0", RegisterMMX },
    { "mm1", RegisterMMX },
    { "mm2", RegisterMMX },
    { "mm3", RegisterMMX },
    { "mm4", RegisterMMX },
    { "mm5", RegisterMMX },
    { "mm6", RegisterMMX },
    { "mm7", RegisterMMX }
 };

static RegisterType guessType(const QByteArray &name)
{
    static QHash<QByteArray, RegisterType> theTypes;
    if (theTypes.isEmpty()) {
        for (int i = 0; i != sizeof(theNameAndType) / sizeof(theNameAndType[0]); ++i)
            theTypes[theNameAndType[i].name] = theNameAndType[i].type;
    }
    return theTypes.value(name, RegisterUnknown);
}

static int childCountFromType(int type)
{
    switch (type) {
        case RegisterUnknown: return 0;
        case RegisterI8: return 0;
        case RegisterI16: return 1;
        case RegisterI32: return 2;
        case RegisterI64: return 3;
        case RegisterI128: return 4;
        case RegisterF32: return 0;
        case RegisterF64: return 0;
        case RegisterF80: return 0;
        case RegisterXMM: return 3;
        case RegisterMMX: return 3;
        case RegisterNeon: return 3;
        case RegisterFlags32: return 0;
    }
    QTC_ASSERT(false, /**/);
    return 0;
}

static int bitWidthFromType(int type, int subType)
{
    const uint integer[] = { 8, 16, 32, 64, 128 };
    const uint xmm[] = { 8, 16, 32, 64, 128 };
    const uint mmx[] = { 8, 16, 32, 64, 128 };
    const uint neon[] = { 8, 16, 32, 64, 128 };

    switch (type) {
        case RegisterUnknown: return 0;
        case RegisterI8: return 8;
        case RegisterI16: return integer[subType];
        case RegisterI32: return integer[subType];
        case RegisterI64: return integer[subType];
        case RegisterI128: return integer[subType];
        case RegisterF32: return 0;
        case RegisterF64: return 0;
        case RegisterF80: return 0;
        case RegisterXMM: return xmm[subType];
        case RegisterMMX: return mmx[subType];
        case RegisterNeon: return neon[subType];
        case RegisterFlags32: return 0;
    }
    QTC_ASSERT(false, /**/);
    return 0;
}

static const uint TopLevelId = UINT_MAX;
static bool isTopLevelItem(const QModelIndex &index)
{
    return quintptr(index.internalId()) == quintptr(TopLevelId);
}

Register::Register(const QByteArray &name_)
    : name(name_), changed(true)
{
    type = guessType(name);
}


//////////////////////////////////////////////////////////////////
//
// RegisterHandler
//
//////////////////////////////////////////////////////////////////

RegisterHandler::RegisterHandler()
{
    setObjectName(QLatin1String("RegisterModel"));
    m_prettyOutput = 0;
    m_base = 16;
    calculateWidth();
#if USE_REGISTER_MODEL_TEST
    new ModelTest(this, 0);
#endif
}

int RegisterHandler::rowCount(const QModelIndex &idx) const
{
    if (idx.column() > 0)
        return 0;
    if (!idx.isValid())
        return m_registers.size(); // Top level.
    if (!isTopLevelItem(idx))
        return 0; // Sub-Items don't have children.
    if (idx.row() >= m_registers.size())
        return 0;
    return childCountFromType(m_registers[idx.row()].type);
}

int RegisterHandler::columnCount(const QModelIndex &idx) const
{
    if (idx.column() > 0)
        return 0;
    if (!idx.isValid())
        return 2;
    if (!isTopLevelItem(idx))
        return 0; // Sub-Items don't have children.
    return 2;
}

QModelIndex RegisterHandler::index(int row, int col, const QModelIndex &parent) const
{
    if (row < 0 || col < 0 || col >= 2)
        return QModelIndex();
    if (!parent.isValid()) // Top level.
        return createIndex(row, col, TopLevelId);
    if (!isTopLevelItem(parent)) // Sub-Item has no children.
        return QModelIndex();
    if (parent.column() > 0)
        return QModelIndex();
    return createIndex(row, col, parent.row());
}

QModelIndex RegisterHandler::parent(const QModelIndex &idx) const
{
    if (!idx.isValid())
        return QModelIndex();
    if (!isTopLevelItem(idx))
        return createIndex(idx.internalId(), 0, TopLevelId);
    return QModelIndex();
}

// Editor value: Preferably number, else string.
QVariant Register::editValue() const
{
    bool ok = true;
    // Try to convert to number?
    const qulonglong v = value.toULongLong(&ok, 0); // Autodetect format
    if (ok)
        return QVariant(v);
    return QVariant(value);
}

// Editor value: Preferably padded number, else padded string.
QString Register::displayValue(int base, int strlen) const
{
    const QVariant editV = editValue();
    if (editV.type() == QVariant::ULongLong)
        return QString::fromLatin1("%1").arg(editV.toULongLong(), strlen, base, QLatin1Char('0'));
    const QString stringValue = editV.toString();
    if (stringValue.size() < strlen)
        return QString(strlen - stringValue.size(), QLatin1Char(' ')) + QLatin1String(value);
    return stringValue;
}

QVariant RegisterHandler::data(const QModelIndex &index, int role) const
{
    if (!index.isValid())
        return QVariant();

    QModelIndex topLevel = index.parent();
    const int mainRow = topLevel.isValid() ? topLevel.row() : index.row();

    if (mainRow >= m_registers.size())
        return QVariant();


    const Register &reg = m_registers.at(mainRow);

    if (topLevel.isValid()) {
        //
        // Nested
        //
        int subType = index.row();
        int bitWidth = bitWidthFromType(reg.type, subType);

        switch (role) {
        case Qt::DisplayRole:
            switch (index.column()) {
            case 0: {
                switch (bitWidth) {
                    case 8:  return QLatin1String("[Bytes]");
                    case 16: return QLatin1String("[Words]");
                    case 32: return QLatin1String("[DWords]");
                    case 64: return QLatin1String("[QWords]");
                    case 128: return QLatin1String("[TWords]");
                    case -32: return QLatin1String("[Single]");
                    case -64: return QLatin1String("[Double]");
                    return QVariant(bitWidth);
                }
            }
        }
        default:
            break;
        }

    } else {
        //
        // Toplevel
        //

        switch (role) {
        case Qt::DisplayRole:
            switch (index.column()) {
            case 0: {
                const QString padding = QLatin1String("  ");
                return QVariant(padding + QLatin1String(reg.name) + padding);
                //return QVariant(reg.name);
            }
            case 1: // Display: Pad value for alignment
                return reg.displayValue(m_base, m_strlen);
            } // switch column
        case Qt::EditRole: // Edit: Unpadded for editing
            return reg.editValue();
        case Qt::TextAlignmentRole:
            return index.column() == 1 ? QVariant(Qt::AlignRight) : QVariant();
        default:
            break;
        }
    }
    return QVariant();
}

QVariant RegisterHandler::headerData(int section, Qt::Orientation orientation,
    int role) const
{
    if (orientation == Qt::Horizontal && role == Qt::DisplayRole) {
        switch (section) {
        case 0: return tr("Name");
        case 1: return tr("Value (Base %1)").arg(m_base);
        };
    }
    return QVariant();
}

Qt::ItemFlags RegisterHandler::flags(const QModelIndex &idx) const
{
    if (!idx.isValid())
        return Qt::ItemFlags();

    const Qt::ItemFlags notEditable = Qt::ItemIsSelectable|Qt::ItemIsEnabled;
    // Can edit registers if they are hex numbers and not arrays.
    if (idx.column() == 1
            && IntegerWatchLineEdit::isUnsignedHexNumber(QLatin1String(m_registers.at(idx.row()).value)))
        return notEditable | Qt::ItemIsEditable;
    return notEditable;
}

void RegisterHandler::writePrettyOutput(const Registers &registers)
{
    if( !m_prettyOutput )
        return;

    int oldPos = m_prettyOutput->verticalScrollBar()->value();
    m_prettyOutput->clear();

    Registers::const_iterator oldIt = m_registers.constBegin();

    Registers::const_iterator itor = registers.constBegin();
    Registers::const_iterator end  = registers.constEnd();

    int newLineCount = 0;

    QColor textColours[2];
    textColours[0] = QColor( 0, 0, 0 );
    textColours[1] = QColor( 200, 0, 0 );
    int currentColour = 0;

    while( itor != end )
    {
        const Register &reg = *itor;
        const Register &oldReg = *oldIt;

        //xmm registers go last, so we can pair them with their subchildren
        if( reg.type != RegisterXMM )
        {
            bool changed = reg.value != oldReg.value;

            if( changed && !currentColour )
            {
                currentColour = 1;
                m_prettyOutput->setTextColor( textColours[currentColour] );
            }
            else if( !changed && currentColour )
            {
                currentColour = 0;
                m_prettyOutput->setTextColor( textColours[currentColour] );
            }

            m_prettyOutput->insertPlainText( QLatin1String( reg.name ) );
            m_prettyOutput->insertPlainText( QLatin1String( " = ") );

            if( reg.type != RegisterF80 )
            {
                m_prettyOutput->insertPlainText( QLatin1String( reg.value ) );
            }
            else
            {
                //Floating point
                int start = reg.value.indexOf( 'x' );
                start = start == -1 ? 0 : (start + 1);

                //Convert 80-bit registers to 64-bit representation (we loose a bit of
                //precision, users can use the other register window which outputs in hex)
                QString subReg;
                subReg = QLatin1String( reg.value.mid( start, 16 ) );

                union
                {
                    //Strict aliasing extension (any sane compiler should allow this)
                    quint64 valueU64;
                    double  valueDouble;
                };

                bool ok;
                valueU64 = subReg.toLongLong( &ok, 16 );

                Q_ASSERT( ok );

                if( valueU64 == 0x7ff0000000000000 )
                {
                    m_prettyOutput->insertPlainText( QLatin1String( "###  Inf  ###") );
                }
                else if( valueU64 == 0xfff0000000000000 )
                {
                    m_prettyOutput->insertPlainText( QLatin1String( "### -Inf  ###") );
                }
                else if( ( (valueU64 & 0x7ff0000000000000) == 0x7ff0000000000000 ) &&
                         (valueU64 &   0x000fffffffffffff) )
                {
                    m_prettyOutput->insertPlainText( QLatin1String( "###  NaN  ###") );
                }
                else
                {
                    //Positive values have a leading space, negative values start with '-'
                    m_prettyOutput->insertPlainText( QLatin1String( valueU64 & 0x80000000000000 ?
                                                                                "" : " " ) +
                                                     QString::number( valueDouble, 'e', 14 ) +
                                                     QLatin1String( " | ") +
                                                     QLatin1String( reg.value ) );
                }
            }

            ++newLineCount;

            //> 80-bit registers are very long, one line for each
            if( reg.type == RegisterI128 || reg.type == RegisterF80 )
                newLineCount = 0;
            else if( reg.type == RegisterI16 || reg.type == RegisterI8 )
                newLineCount = newLineCount % 4;
            else
                newLineCount = newLineCount % 2;

            m_prettyOutput->insertPlainText( QLatin1String( newLineCount ? "   " : "\n" ) );
        }

        ++oldIt;
        ++itor;
    }

    m_prettyOutput->insertPlainText( QLatin1String( "\n" ) );

    //Again, but this time only XMM registers
    oldIt= m_registers.constBegin();
    itor = registers.constBegin();

    while( itor != end )
    {
        const Register &reg = *itor;
        const Register &oldReg = *oldIt;

        if( reg.type == RegisterXMM )
        {
            bool changed = reg.value != oldReg.value;

            if( changed && !currentColour )
            {
                currentColour = 1;
                m_prettyOutput->setTextColor( textColours[currentColour] );
            }
            else if( !changed && currentColour )
            {
                currentColour = 0;
                m_prettyOutput->setTextColor( textColours[currentColour] );
            }

            m_prettyOutput->append( QLatin1String( reg.name ) );
            m_prettyOutput->insertPlainText( QLatin1String( " = ") );
            m_prettyOutput->insertPlainText( QLatin1String( reg.value ) );
        }

        ++oldIt;
        ++itor;
    }

    m_prettyOutput->insertPlainText( QLatin1String( "\n" ) );


    //Again! but only output children xmm registers (i.e. xmm00)
    oldIt= m_registers.constBegin();
    itor = registers.constBegin();

    while( itor != end )
    {
        const Register &reg     = *itor;
        const Register &oldReg  = *oldIt;

        if( reg.type == RegisterXMM )
        {
            //Reinterpret as 4 floats
            int start = reg.value.indexOf( 'x' );
            start = start == -1 ? 0 : (start + 1);

            for( size_t i=0; i<4; ++i )
            {
                QString subReg, oldSubReg;
                subReg      = QLatin1String( reg.value.mid( start + (3-i) * 8, 8 ) );
                oldSubReg   = QLatin1String( oldReg.value.mid( start + (3-i) * 8, 8 ) );

                union
                {
                    //Strict aliasing extension (any sane compiler should allow this)
                    quint32  valueU32;
                    float   valueFloat;
                };

                bool ok;
                quint32 oldValue= oldSubReg.toUInt( &ok, 16 );
                valueU32        = subReg.toUInt( &ok, 16 );

                Q_ASSERT( ok );

                bool changed = valueU32 != oldValue;
                if( changed && !currentColour )
                {
                    currentColour = 1;
                    m_prettyOutput->setTextColor( textColours[currentColour] );
                }
                else if( !changed && currentColour )
                {
                    currentColour = 0;
                    m_prettyOutput->setTextColor( textColours[currentColour] );
                }

                m_prettyOutput->insertPlainText( QLatin1String( reg.name ) + QString::number( i ) );
                m_prettyOutput->insertPlainText( QLatin1String( " = ") );

                if( valueU32 == 0x7f800000 )
                    m_prettyOutput->insertPlainText( QLatin1String( "###  Inf  ###  ") );
                else if( valueU32 == 0xff800000 )
                    m_prettyOutput->insertPlainText( QLatin1String( "### -Inf  ###  ") );
                else if( ( (valueU32 & 0x7f800000) == 0x7f800000 ) && (valueU32 & 0x007fffff) )
                    m_prettyOutput->insertPlainText( QLatin1String( "###  NaN  ###  ") );
                else
                {
                    //Positive values have a leading space, negative values start with '-'
                    m_prettyOutput->insertPlainText( QLatin1String( valueU32 & 0x80000000 ? "" : " " ) +
                                                     QString::number( valueFloat, 'e' ) +
                                                     QLatin1String( "  ") );
                }
            }

            m_prettyOutput->insertPlainText( QLatin1String( "\n" ) );
        }

        ++oldIt;
        ++itor;
    }

    m_prettyOutput->verticalScrollBar()->setValue( oldPos );
}

void RegisterHandler::removeAll()
{
    beginResetModel();
    m_registers.clear();
    endResetModel();
}

bool RegisterHandler::isEmpty() const
{
    return m_registers.isEmpty();
}

// Compare register sets by name
static inline bool compareRegisterSet(const Registers &r1, const Registers &r2)
{
    if (r1.size() != r2.size())
        return false;
    const int size = r1.size();
    for (int r = 0; r < size; r++)
        if (r1.at(r).name != r2.at(r).name)
            return false;
    return true;
}

void RegisterHandler::setRegisters(const Registers &registers)
{
    beginResetModel();
    m_registers = registers;
    const int size = m_registers.size();
    for (int r = 0; r < size; r++)
        m_registers[r].changed = false;
    calculateWidth();
    endResetModel();
}

void RegisterHandler::setAndMarkRegisters(const Registers &registers)
{
    if (!compareRegisterSet(m_registers, registers)) {
        setRegisters(registers);
        writePrettyOutput(registers);
        return;
    }

    writePrettyOutput(registers);

    const int size = m_registers.size();
    for (int r = 0; r != size; ++r) {
        const QModelIndex regIndex = index(r, 1, QModelIndex());
        if (m_registers.at(r).value != registers.at(r).value) {
            // Indicate red if values change, keep changed.
            m_registers[r].changed = m_registers.at(r).changed
                || !m_registers.at(r).value.isEmpty();
            m_registers[r].value = registers.at(r).value;
            emit dataChanged(regIndex, regIndex);
        }
        emit registerSet(regIndex); // Notify attached memory views.
    }
}

Registers RegisterHandler::registers() const
{
    return m_registers;
}

void RegisterHandler::calculateWidth()
{
    m_strlen = (m_base == 2 ? 64 : m_base == 8 ? 32 : m_base == 10 ? 26 : 16);
}

void RegisterHandler::setNumberBase(int base)
{
    if (m_base != base) {
        beginResetModel();
        m_base = base;
        calculateWidth();
        endResetModel();
    }
}

} // namespace Internal
} // namespace Debugger
