ChunkData DiffUtils::calculateOriginalData(const QList<Diff> &leftDiffList,
            if (i < leftDiffList.count() || j < rightDiffList.count() || (leftLines.count() && rightLines.count())) {
                while (line < qMax(newLeftLines.count(), newRightLines.count())) {
                    handleLine(newLeftLines, line, &leftLines, &leftLineNumber);
                    handleLine(newRightLines, line, &rightLines, &rightLineNumber);

                    const int commonLineCount = qMin(newLeftLines.count(),
                                                     newRightLines.count());
                    if (line < commonLineCount) {
                        // try to align
                        const int leftDifference = leftLineNumber - leftLineAligned;
                        const int rightDifference = rightLineNumber - rightLineAligned;

                        if (leftDifference && rightDifference) {
                            bool doAlign = true;
                            if (line == 0
                                    && (newLeftLines.at(0).isEmpty()
                                        || newRightLines.at(0).isEmpty())
                                    && !lastLineEqual) {
                                // omit alignment when first lines of equalities
                                // are empty and last generated lines are not equal
                            }

                            if (line == commonLineCount - 1) {
                                // omit alignment when last lines of equalities are empty
                                if (leftLines.last().text.isEmpty()
                                        || rightLines.last().text.isEmpty())
                                    doAlign = false;

                                // unless it's the last dummy line (don't omit in that case)
                                if (i == leftDiffList.count()
                                        && j == rightDiffList.count())
                                    doAlign = true;
                            }

                            if (doAlign) {
                                // align here
                                leftLineAligned = leftLineNumber;
                                rightLineAligned = rightLineNumber;

                                // insert separators if needed
                                if (rightDifference > leftDifference)
                                    leftSpans.insert(leftLineNumber,
                                                     rightDifference - leftDifference);
                                else if (leftDifference > rightDifference)
                                    rightSpans.insert(rightLineNumber,
                                                      leftDifference - rightDifference);
                            }
                    // check if lines are equal
                    if ((line < commonLineCount - 1) // before the last common line in equality
                            || (line == commonLineCount - 1 // or the last common line in equality
                                && i == leftDiffList.count() // and it's the last iteration
                                && j == rightDiffList.count())) {
                        if (line > 0 || lastLineEqual)
                            equalLines.insert(leftLineNumber, rightLineNumber);
                    }
                    if (line > 0)
                        lastLineEqual = true;
                    line++;
                }
    ChunkData chunkData;

FileData DiffUtils::calculateContextData(const ChunkData &originalData,
                                         int contextLinesNumber,
                                         int joinChunkThreshold)
    fileData.contextChunksIncluded = true;

            const int firstLine = first
                    ? 0 : equalRowStart + contextLinesNumber;
            const int lastLine = last
                    ? originalData.rows.count() : i - contextLinesNumber;
    int leftLineNumber = 0;
    int rightLineNumber = 0;
        chunkData.leftStartingLineNumber = leftLineNumber;
        chunkData.rightStartingLineNumber = rightLineNumber;
            if (rowData.leftLine.textLineType == TextLineData::TextLine)
                ++leftLineNumber;
            if (rowData.rightLine.textLineType == TextLineData::TextLine)
                ++rightLineNumber;
QString DiffUtils::makePatchLine(const QChar &startLineCharacter,
                          const QString &textLine,
                          bool lastChunk,
                          bool lastLine)
{
    QString line;

    const bool addNoNewline = lastChunk // it's the last chunk in file
            && lastLine // it's the last row in chunk
            && !textLine.isEmpty(); // the row is not empty

    const bool addLine = !lastChunk // not the last chunk in file
            || !lastLine // not the last row in chunk
            || addNoNewline; // no addNoNewline case

    if (addLine) {
        line = startLineCharacter + textLine + QLatin1Char('\n');
        if (addNoNewline)
            line += QLatin1String("\\ No newline at end of file\n");
    }

    return line;
}

QString DiffUtils::makePatch(const ChunkData &chunkData,
                  const QString &leftFileName,
                  const QString &rightFileName,
                  bool lastChunk)
{
    QString diffText;
    int leftLineCount = 0;
    int rightLineCount = 0;
    QList<TextLineData> leftBuffer, rightBuffer;

    for (int i = 0; i <= chunkData.rows.count(); i++) {
        const RowData &rowData = i < chunkData.rows.count()
                ? chunkData.rows.at(i)
                : RowData(TextLineData(TextLineData::Separator)); // dummy,
                                        // ensure we process buffers to the end.
                                        // rowData will be equal
        if (rowData.equal) {
            if (leftBuffer.count()) {
                for (int j = 0; j < leftBuffer.count(); j++) {
                    const QString line = makePatchLine(QLatin1Char('-'),
                                              leftBuffer.at(j).text,
                                              lastChunk,
                                              i == chunkData.rows.count()
                                                       && j == leftBuffer.count() - 1);

                    if (!line.isEmpty())
                        ++leftLineCount;

                    diffText += line;
                }
                leftBuffer.clear();
            }
            if (rightBuffer.count()) {
                for (int j = 0; j < rightBuffer.count(); j++) {
                    const QString line = makePatchLine(QLatin1Char('+'),
                                              rightBuffer.at(j).text,
                                              lastChunk,
                                              i == chunkData.rows.count()
                                                       && j == rightBuffer.count() - 1);

                    if (!line.isEmpty())
                        ++rightLineCount;

                    diffText += line;
                }
                rightBuffer.clear();
            }
            if (i < chunkData.rows.count()) {
                const QString line = makePatchLine(QLatin1Char(' '),
                                          rowData.rightLine.text,
                                          lastChunk,
                                          i == chunkData.rows.count() - 1);

                if (!line.isEmpty()) {
                    ++leftLineCount;
                    ++rightLineCount;
                }

                diffText += line;
            }
        } else {
            if (rowData.leftLine.textLineType == TextLineData::TextLine)
                leftBuffer.append(rowData.leftLine);
            if (rowData.rightLine.textLineType == TextLineData::TextLine)
                rightBuffer.append(rowData.rightLine);
        }
    }

    const QString chunkLine = QLatin1String("@@ -")
            + QString::number(chunkData.leftStartingLineNumber + 1)
            + QLatin1Char(',')
            + QString::number(leftLineCount)
            + QLatin1String(" +")
            + QString::number(chunkData.rightStartingLineNumber + 1)
            + QLatin1Char(',')
            + QString::number(rightLineCount)
            + QLatin1String(" @@\n");

    diffText.prepend(chunkLine);

    const QString rightFileInfo = QLatin1String("+++ ") + rightFileName + QLatin1Char('\n');
    const QString leftFileInfo = QLatin1String("--- ") + leftFileName + QLatin1Char('\n');

    diffText.prepend(rightFileInfo);
    diffText.prepend(leftFileInfo);

    return diffText;
}

static QList<RowData> readLines(const QString &patch,
                                bool ignoreWhitespace,
                                bool lastChunk,
                                bool *lastChunkAtTheEndOfFile,
                                bool *ok)
{
//    const QRegExp lineRegExp(QLatin1String("(?:\\n)"                                      // beginning of the line
//                                           "([ \\-\\+\\\\])([^\\n]*)"                     // -, +, \\ or space, followed by any no-newline character
//                                           "(?:\\n|$)"));                                 // end of line or file
    QList<Diff> diffList;

    const QChar newLine = QLatin1Char('\n');

    int lastEqual = -1;
    int lastDelete = -1;
    int lastInsert = -1;

    int noNewLineInEqual = -1;
    int noNewLineInDelete = -1;
    int noNewLineInInsert = -1;

    const QStringList lines = patch.split(newLine);
    int i;
    for (i = 0; i < lines.count(); i++) {
        const QString line = lines.at(i);
        if (line.isEmpty())
            break; // need to have at least one character (1 column)
        QChar firstCharacter = line.at(0);
        if (firstCharacter == QLatin1Char('\\')) { // no new line marker
            if (!lastChunk) // can only appear in last chunk of the file
                break;
            if (!diffList.isEmpty()) {
                Diff &last = diffList.last();
                if (last.text.isEmpty())
                    break;
                if (last.text.at(0) == newLine) // there is a new line
                    break;

                if (last.command == Diff::Equal) {
                    if (noNewLineInEqual >= 0)
                        break;
                    noNewLineInEqual = diffList.count() - 1;
                } else if (last.command == Diff::Delete) {
                    if (noNewLineInDelete >= 0)
                        break;
                    noNewLineInDelete = diffList.count() - 1;
                } else if (last.command == Diff::Insert) {
                    if (noNewLineInInsert >= 0)
                        break;
                    noNewLineInInsert = diffList.count() - 1;
                }
            }
        } else {
            Diff::Command command = Diff::Equal;
            if (firstCharacter == QLatin1Char(' ')) // common line
                command = Diff::Equal;
            else if (firstCharacter == QLatin1Char('-')) // deleted line
                command = Diff::Delete;
            else if (firstCharacter == QLatin1Char('+')) // inserted line
                command = Diff::Insert;
            else
                break; // no other character may exist as the first character

            Diff diffToBeAdded(command, line.mid(1) + newLine);

            if (!diffList.isEmpty() && diffList.last().command == command)
                diffList.last().text.append(diffToBeAdded.text);
            else
                diffList.append(diffToBeAdded);

            if (command == Diff::Equal) // common line
                lastEqual = diffList.count() - 1;
            else if (command == Diff::Delete) // deleted line
                lastDelete = diffList.count() - 1;
            else if (command == Diff::Insert) // inserted line
                lastInsert = diffList.count() - 1;
        }
    }

    if (i < lines.count()
            || (noNewLineInEqual >= 0 && (noNewLineInDelete >= 0 || noNewLineInInsert >= 0))
            || (noNewLineInEqual >= 0 && noNewLineInEqual != lastEqual)
            || (noNewLineInDelete >= 0 && noNewLineInDelete != lastDelete)
            || (noNewLineInInsert >= 0 && noNewLineInInsert != lastInsert)) {
        if (ok)
            *ok = false;
        return QList<RowData>();
    }

    if (ok)
        *ok = true;

    bool removeNewLineFromLastEqual = false;
    bool removeNewLineFromLastDelete = false;
    bool removeNewLineFromLastInsert = false;
    bool prependNewLineAfterLastEqual = false;

    if (noNewLineInDelete >= 0 || noNewLineInInsert >= 0) {
        if (noNewLineInDelete >= 0)
            removeNewLineFromLastDelete = true;
        if (noNewLineInInsert >= 0)
            removeNewLineFromLastInsert = true;
    } else if (lastEqual > lastDelete && lastEqual > lastInsert) {
        removeNewLineFromLastEqual = true;
    } else if (lastDelete > lastEqual && lastDelete > lastInsert) {
        if (lastInsert > lastEqual) {
            removeNewLineFromLastDelete = true;
            removeNewLineFromLastInsert = true;
        } else if (lastEqual > lastInsert) {
            removeNewLineFromLastEqual = true;
            prependNewLineAfterLastEqual = true;
        }
    } else if (lastInsert > lastEqual && lastInsert > lastDelete) {
        if (lastDelete > lastEqual) {
            removeNewLineFromLastDelete = true;
            removeNewLineFromLastInsert = true;
        } else if (lastEqual > lastDelete) {
            removeNewLineFromLastEqual = true;
            prependNewLineAfterLastEqual = true;
        }
    }

    if (removeNewLineFromLastEqual) {
        Diff &diff = diffList[lastEqual];
        diff.text = diff.text.left(diff.text.count() - 1);
    }
    if (removeNewLineFromLastDelete) {
        Diff &diff = diffList[lastDelete];
        diff.text = diff.text.left(diff.text.count() - 1);
    }
    if (removeNewLineFromLastInsert) {
        Diff &diff = diffList[lastInsert];
        diff.text = diff.text.left(diff.text.count() - 1);
    }
    if (prependNewLineAfterLastEqual) {
        Diff &diff = diffList[lastEqual + 1];
        diff.text = newLine + diff.text;
    }

    if (lastChunkAtTheEndOfFile) {
        *lastChunkAtTheEndOfFile = noNewLineInEqual >= 0
                || noNewLineInDelete >= 0|| noNewLineInInsert >= 0;
    }

//    diffList = Differ::merge(diffList);
    QList<Diff> leftDiffList;
    QList<Diff> rightDiffList;
    Differ::splitDiffList(diffList, &leftDiffList, &rightDiffList);
    QList<Diff> outputLeftDiffList;
    QList<Diff> outputRightDiffList;

    if (ignoreWhitespace) {
        const QList<Diff> leftIntermediate =
                Differ::moveWhitespaceIntoEqualities(leftDiffList);
        const QList<Diff> rightIntermediate =
                Differ::moveWhitespaceIntoEqualities(rightDiffList);
        Differ::ignoreWhitespaceBetweenEqualities(leftIntermediate,
                                                  rightIntermediate,
                                                  &outputLeftDiffList,
                                                  &outputRightDiffList);
    } else {
        Differ::diffBetweenEqualities(leftDiffList,
                                      rightDiffList,
                                      &outputLeftDiffList,
                                      &outputRightDiffList);
    }

    return DiffUtils::calculateOriginalData(outputLeftDiffList,
                                            outputRightDiffList).rows;
}

static QList<ChunkData> readChunks(const QString &patch,
                                   bool ignoreWhitespace,
                                   bool *lastChunkAtTheEndOfFile,
                                   bool *ok)
{
    const QRegExp chunkRegExp(QLatin1String("((?:\\n|^)"                                      // beginning of the line
                                            "@@ \\-([\\d]+)\\,[\\d]+ \\+([\\d]+)\\,[\\d]+ @@" // @@ -leftPos,leftCount +rightPos,rightCount @@
                                            "(?:\\ +[^\\n]*)?"                                // optional hint (e.g. function name)
                                            "(?:\\n))"));                                     // end of line (need to be followed by text line)

    bool readOk = false;

    QList<ChunkData> chunkDataList;

    int pos = chunkRegExp.indexIn(patch, 0);
    if (pos == 0) {
        int endOfLastChunk = 0;
        do {
            const QStringList capturedTexts = chunkRegExp.capturedTexts();
            const QString captured = capturedTexts.at(1);
            const int leftStartingPos = capturedTexts.at(2).toInt();
            const int rightStartingPos = capturedTexts.at(3).toInt();
            if (endOfLastChunk > 0) {
                const QString lines = patch.mid(endOfLastChunk,
                                                pos - endOfLastChunk);
                chunkDataList.last().rows = readLines(lines,
                                                      ignoreWhitespace,
                                                      false,
                                                      lastChunkAtTheEndOfFile,
                                                      &readOk);
                if (!readOk)
                    break;
            }
            pos += captured.count();
            endOfLastChunk = pos;
            ChunkData chunkData;
            chunkData.leftStartingLineNumber = leftStartingPos - 1;
            chunkData.rightStartingLineNumber = rightStartingPos - 1;
            chunkDataList.append(chunkData);
        } while ((pos = chunkRegExp.indexIn(patch, pos)) != -1);

        if (endOfLastChunk > 0) {
            const QString lines = patch.mid(endOfLastChunk);
            chunkDataList.last().rows = readLines(lines,
                                                  ignoreWhitespace,
                                                  true,
                                                  lastChunkAtTheEndOfFile,
                                                  &readOk);
        }
    }

    if (ok)
        *ok = readOk;

    return chunkDataList;
}

static FileData readDiffHeaderAndChunks(const QString &headerAndChunks,
                                        bool ignoreWhitespace,
                                        bool *ok)
{
    QString patch = headerAndChunks;
    FileData fileData;
    bool readOk = false;

    const QRegExp leftFileRegExp(QLatin1String("((?:\\n|^)\\-{3} ")        // "--- "
                                 + QLatin1String("([^\\t\\n]+)")            // "fileName1"
                                 + QLatin1String("(?:\\t[^\\n]*)*\\n)"));  // optionally followed by: \t anything \t anything ...)
    const QRegExp rightFileRegExp(QLatin1String("(^\\+{3} ")               // "+++ "
                                  + QLatin1String("([^\\t\\n]+)")           // "fileName2"
                                  + QLatin1String("(?:\\t[^\\n]*)*\\n)")); // optionally followed by: \t anything \t anything ...)
    const QRegExp binaryRegExp(QLatin1String("(^Binary files ")
                               + QLatin1String("([^\\t\\n]+)")
                               + QLatin1String(" and ")
                               + QLatin1String("([^\\t\\n]+)")
                               + QLatin1String(" differ$)"));

    // followed either by leftFileRegExp or by binaryRegExp
    if (leftFileRegExp.indexIn(patch, 0) == 0) {
        const QStringList leftCaptured = leftFileRegExp.capturedTexts();
        patch = patch.mid(leftCaptured.at(1).count());
        fileData.leftFileInfo.fileName = leftCaptured.at(2);

        // followed by rightFileRegExp
        if (rightFileRegExp.indexIn(patch, 0) == 0) {
            const QStringList rightCaptured = rightFileRegExp.capturedTexts();
            patch = patch.mid(rightCaptured.at(1).count());
            fileData.rightFileInfo.fileName = rightCaptured.at(2);

            fileData.chunks = readChunks(patch,
                                         ignoreWhitespace,
                                         &fileData.lastChunkAtTheEndOfFile,
                                         &readOk);
        }
    } else if (binaryRegExp.indexIn(patch, 0) == 0) {
        const QStringList binaryCaptured = binaryRegExp.capturedTexts();
        fileData.leftFileInfo.fileName = binaryCaptured.at(2);
        fileData.rightFileInfo.fileName = binaryCaptured.at(3);
        fileData.binaryFiles = true;
        readOk = true;
    }

    if (ok)
        *ok = readOk;

    if (!readOk)
        return FileData();

    return fileData;

}

static QList<FileData> readDiffPatch(const QString &patch,
                                     bool ignoreWhitespace,
                                     bool *ok)
    const QRegExp diffRegExp(QLatin1String("("                  // capture all
                                           "(?:\\n|^)"          // new line of the beginning of a patch
                                           "("                  // either
                                           "\\-{3} "            // ---
                                           "[^\\t\\n]+"         // filename1
                                           "(?:\\t[^\\n]*)*\\n" // optionally followed by: \t anything \t anything ...
                                           "\\+{3} "            // +++
                                           "[^\\t\\n]+"         // filename2
                                           "(?:\\t[^\\n]*)*\\n" // optionally followed by: \t anything \t anything ...
                                           "|"                  // or
                                           "Binary files "
                                           "[^\\t\\n]+"         // filename1
                                           " and "
                                           "[^\\t\\n]+"         // filename2
                                           " differ"
                                           ")"                  // end of or
                                           ")"));               // end of capture all

    bool readOk = false;

    QList<FileData> fileDataList;

    int pos = diffRegExp.indexIn(patch, 0);
    if (pos == 0) { // git style patch
        readOk = true;
        int lastPos = -1;
        do {
            const QStringList capturedTexts = diffRegExp.capturedTexts();
            const QString captured = capturedTexts.at(1);
            if (lastPos >= 0) {
                const QString headerAndChunks = patch.mid(lastPos,
                                                          pos - lastPos);

                const FileData fileData = readDiffHeaderAndChunks(headerAndChunks,
                                                                  ignoreWhitespace,
                                                                  &readOk);

                if (!readOk)
                    break;

                fileDataList.append(fileData);
            }
            lastPos = pos;
            pos += captured.count();
        } while ((pos = diffRegExp.indexIn(patch, pos)) != -1);

        if (lastPos >= 0 && readOk) {
            const QString headerAndChunks = patch.mid(lastPos,
                                                      patch.count() - lastPos - 1);

            const FileData fileData = readDiffHeaderAndChunks(headerAndChunks,
                                                              ignoreWhitespace,
                                                              &readOk);

            if (readOk)
                fileDataList.append(fileData);
        }

    if (ok)
        *ok = readOk;

    if (!readOk)
        return QList<FileData>();

    return fileDataList;
static FileData readGitHeaderAndChunks(const QString &headerAndChunks,
                                       const QString &fileName,
                                       bool ignoreWhitespace,
                                       bool *ok)
    FileData fileData;
    fileData.leftFileInfo.fileName = fileName;
    fileData.rightFileInfo.fileName = fileName;

    QString patch = headerAndChunks;
    bool readOk = false;

    const QString devNull(QLatin1String("/dev/null"));

    // will be followed by: index 0000000..shasha, file "a" replaced by "/dev/null", @@ -0,0 +m,n @@
    const QRegExp newFileMode(QLatin1String("(^new file mode [\\d]+\\n)")); // new file mode octal

    // will be followed by: index shasha..0000000, file "b" replaced by "/dev/null", @@ -m,n +0,0 @@
    const QRegExp deletedFileMode(QLatin1String("(^deleted file mode [\\d]+\\n)")); // deleted file mode octal

    const QRegExp indexRegExp(QLatin1String("(^index ([\\w]+)\\.{2}([\\w]+)(?: [\\d]+)?\\n)")); // index cap2..cap3(optionally: octal)

    QString leftFileName = QLatin1String("a/") + fileName;
    QString rightFileName = QLatin1String("b/") + fileName;

    if (newFileMode.indexIn(patch, 0) == 0) {
        fileData.leftFileInfo.devNull = true;
        leftFileName = devNull;
        patch = patch.mid(newFileMode.capturedTexts().at(1).count());
    } else if (deletedFileMode.indexIn(patch, 0) == 0) {
        fileData.rightFileInfo.devNull = true;
        rightFileName = devNull;
        patch = patch.mid(deletedFileMode.capturedTexts().at(1).count());
    }

    if (indexRegExp.indexIn(patch, 0) == 0) {
        const QStringList capturedTexts = indexRegExp.capturedTexts();
        const QString captured = capturedTexts.at(1);
        fileData.leftFileInfo.typeInfo = capturedTexts.at(2);
        fileData.rightFileInfo.typeInfo = capturedTexts.at(3);

        patch = patch.mid(captured.count());

        const QRegExp leftFileRegExp(QLatin1String("(^\\-{3} ")               // "--- "
                                     + leftFileName                           // "a/fileName" or "/dev/null"
                                     + QLatin1String("(?:\\t[^\\n]*)*\\n)")); // optionally followed by: \t anything \t anything ...)
        const QRegExp rightFileRegExp(QLatin1String("(^\\+{3} ")               // "+++ "
                                      + rightFileName                          // "b/fileName" or "/dev/null"
                                      + QLatin1String("(?:\\t[^\\n]*)*\\n)")); // optionally followed by: \t anything \t anything ...)
        const QRegExp binaryRegExp(QLatin1String("(^Binary files ")
                                   + leftFileName
                                   + QLatin1String(" and ")
                                   + rightFileName
                                   + QLatin1String(" differ$)"));

        // followed either by leftFileRegExp or by binaryRegExp
        if (leftFileRegExp.indexIn(patch, 0) == 0) {
            patch = patch.mid(leftFileRegExp.capturedTexts().at(1).count());

            // followed by rightFileRegExp
            if (rightFileRegExp.indexIn(patch, 0) == 0) {
                patch = patch.mid(rightFileRegExp.capturedTexts().at(1).count());

                fileData.chunks = readChunks(patch,
                                             ignoreWhitespace,
                                             &fileData.lastChunkAtTheEndOfFile,
                                             &readOk);
            }
        } else if (binaryRegExp.indexIn(patch, 0) == 0) {
            readOk = true;
            fileData.binaryFiles = true;
        }
    }

    if (ok)
        *ok = readOk;
    if (!readOk)
        return FileData();

    return fileData;
}
static QList<FileData> readGitPatch(const QString &patch, bool ignoreWhitespace, bool *ok)
{
    const QRegExp gitRegExp(QLatin1String("((?:\\n|^)diff --git a/([^\\n]+) b/\\2\\n)")); // diff --git a/cap2 b/cap2

    bool readOk = false;

    QList<FileData> fileDataList;

    int pos = gitRegExp.indexIn(patch, 0);
    if (pos == 0) { // git style patch
        readOk = true;
        int endOfLastHeader = 0;
        QString lastFileName;
        do {
            const QStringList capturedTexts = gitRegExp.capturedTexts();
            const QString captured = capturedTexts.at(1);
            const QString fileName = capturedTexts.at(2);
            if (endOfLastHeader > 0) {
                const QString headerAndChunks = patch.mid(endOfLastHeader,
                                                          pos - endOfLastHeader);

                const FileData fileData = readGitHeaderAndChunks(headerAndChunks,
                                                                 lastFileName,
                                                                 ignoreWhitespace,
                                                                 &readOk);

                if (!readOk)
                    break;
                fileDataList.append(fileData);
            }
            pos += captured.count();
            endOfLastHeader = pos;
            lastFileName = fileName;
        } while ((pos = gitRegExp.indexIn(patch, pos)) != -1);

        if (endOfLastHeader > 0 && readOk) {
            const QString headerAndChunks = patch.mid(endOfLastHeader,
                                                      patch.count() - endOfLastHeader - 1);

            const FileData fileData = readGitHeaderAndChunks(headerAndChunks,
                                                             lastFileName,
                                                             ignoreWhitespace,
                                                             &readOk);

            if (readOk)
                fileDataList.append(fileData);
        }

    if (ok)
        *ok = readOk;

    if (!readOk)
        return QList<FileData>();

    return fileDataList;
QList<FileData> DiffUtils::readPatch(const QString &patch, bool ignoreWhitespace, bool *ok)
    bool readOk = false;

    QList<FileData> fileDataList;

    fileDataList = readGitPatch(patch, ignoreWhitespace, &readOk);
    if (!readOk)
        fileDataList = readDiffPatch(patch, ignoreWhitespace, &readOk);

    if (ok)
        *ok = readOk;

    return fileDataList;