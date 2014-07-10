    QString errorString;
    Utils::TextFileFormat format;
    format.codec = Core::EditorManager::defaultTextCodec();

    QString leftText;
    if (Utils::TextFileFormat::readFile(m_leftFileName,
                                    format.codec,
                                    &leftText, &format, &errorString)
            != Utils::TextFileFormat::ReadSuccess) {

        return;
    }

    QString rightText;
    if (Utils::TextFileFormat::readFile(m_rightFileName,
                                    format.codec,
                                    &rightText, &format, &errorString)
            != Utils::TextFileFormat::ReadSuccess) {

        return;
    }
    if (controller()->isIgnoreWhitespace()) {
                chunkData, controller()->contextLinesNumber(), 0);
    controller()->setDiffFiles(fileDataList);
    QString title = tr("Diff \"%1\", \"%2\"").arg(fileName1).arg(fileName2);
    document = DiffEditorManager::findOrCreate(documentId, title);
    if (!document)
        return;
    DiffEditorController *controller = document->controller();
    if (!controller->reloader()) {
        controller->setReloader(reloader);
    QTest::newRow("Simple not a last chunk") << chunk
                            << fileName
                            << fileName
                            << false
                            << patchText;

    ///////////

    // chunk the same here
    patchText = header + QLatin1String("@@ -1,2 +1,1 @@\n"
                                       "-ABCD\n"
                                       " EFGH\n"
                                       "\\ No newline at end of file\n");

    QTest::newRow("Simple last chunk") << chunk
    rows << RowData(TextLineData(QLatin1String("ABCD")));
    QTest::newRow("EOL in last line removed") << chunk
                                       " ABCD\n"
                                       "-\n");
    QTest::newRow("Last empty line removed") << chunk
    rows << RowData(TextLineData(QLatin1String("ABCD")));
    QTest::newRow("EOL to last line added") << chunk
                                       " ABCD\n"
    QTest::newRow("Last empty line added") << chunk
                            << false
    rows.clear();
    rows << RowData(TextLineData(QLatin1String("ABCD")),
                    TextLineData(QLatin1String("EFGH")));
    rows << RowData(TextLineData(QLatin1String("")));
    chunk.rows = rows;
    QCOMPARE(result, patchText);

    bool ok;
    QList<FileData> resultList = DiffUtils::readPatch(result, false, &ok);

    QVERIFY(ok);
    QCOMPARE(resultList.count(), 1);
    for (int i = 0; i < resultList.count(); i++) {
        const FileData &resultFileData = resultList.at(i);
        QCOMPARE(resultFileData.leftFileInfo.fileName, leftFileName);
        QCOMPARE(resultFileData.rightFileInfo.fileName, rightFileName);
        QCOMPARE(resultFileData.chunks.count(), 1);
        for (int j = 0; j < resultFileData.chunks.count(); j++) {
            const ChunkData &resultChunkData = resultFileData.chunks.at(j);
            QCOMPARE(resultChunkData.leftStartingLineNumber, sourceChunk.leftStartingLineNumber);
            QCOMPARE(resultChunkData.rightStartingLineNumber, sourceChunk.rightStartingLineNumber);
            QCOMPARE(resultChunkData.contextChunk, sourceChunk.contextChunk);
            QCOMPARE(resultChunkData.rows.count(), sourceChunk.rows.count());
            for (int k = 0; k < sourceChunk.rows.count(); k++) {
                const RowData &sourceRowData = sourceChunk.rows.at(k);
                const RowData &resultRowData = resultChunkData.rows.at(k);
                QCOMPARE(resultRowData.equal, sourceRowData.equal);
                QCOMPARE(resultRowData.leftLine.text, sourceRowData.leftLine.text);
                QCOMPARE(resultRowData.leftLine.textLineType, sourceRowData.leftLine.textLineType);
                QCOMPARE(resultRowData.rightLine.text, sourceRowData.rightLine.text);
                QCOMPARE(resultRowData.rightLine.textLineType, sourceRowData.rightLine.textLineType);
            }
        }
    }
                                "diff --git a/file a.txt b/file b.txt\n"
                                "similarity index 99%\n"
                                "copy from file a.txt\n"
                                "copy to file b.txt\n"
                                "index 1234567..9876543\n"
                                "--- a/file a.txt\n"
                                "+++ b/file b.txt\n"
                                "@@ -20,3 +20,3 @@\n"
                                " A\n"
                                "-B\n"
                                "+C\n"
                                " D\n"
                                "diff --git a/file a.txt b/file b.txt\n"
                                "similarity index 99%\n"
                                "rename from file a.txt\n"
                                "rename to file b.txt\n"
                                );
    fileData3.fileOperation = FileData::NewFile;
    fileData4.fileOperation = FileData::DeleteFile;
    fileData5.fileOperation = FileData::NewFile;
    fileData6.fileOperation = FileData::DeleteFile;

    FileData fileData7;
    fileData7.leftFileInfo = DiffFileInfo(QLatin1String("file a.txt"), QLatin1String("1234567"));
    fileData7.rightFileInfo = DiffFileInfo(QLatin1String("file b.txt"), QLatin1String("9876543"));
    fileData7.fileOperation = FileData::CopyFile;
    ChunkData chunkData7;
    chunkData7.leftStartingLineNumber = 19;
    chunkData7.rightStartingLineNumber = 19;
    QList<RowData> rows7;
    rows7.append(RowData(TextLineData(QLatin1String("A"))));
    rows7.append(RowData(TextLineData(QLatin1String("B")),
                         TextLineData(QLatin1String("C"))));
    rows7.append(RowData(TextLineData(QLatin1String("D"))));
    chunkData7.rows = rows7;
    fileData7.chunks.append(chunkData7);

    FileData fileData8;
    fileData8.leftFileInfo = DiffFileInfo(QLatin1String("file a.txt"));
    fileData8.rightFileInfo = DiffFileInfo(QLatin1String("file b.txt"));
    fileData8.fileOperation = FileData::RenameFile;

    QList<FileData> fileDataList1;
    fileDataList1 << fileData1 << fileData2 << fileData3 << fileData4 << fileData5 << fileData6 << fileData7 << fileData8;
                               << fileDataList1;

    //////////////

    patch = QLatin1String("diff --git a/file foo.txt b/file foo.txt\n"
                          "index 1234567..9876543 100644\n"
                          "--- a/file foo.txt\n"
                          "+++ b/file foo.txt\n"
                          "@@ -50,4 +50,5 @@ void DiffEditor::ctor()\n"
                          " A\n"
                          " B\n"
                          " C\n"
                          "+\n");

    fileData1.leftFileInfo = DiffFileInfo(QLatin1String("file foo.txt"), QLatin1String("1234567"));
    fileData1.rightFileInfo = DiffFileInfo(QLatin1String("file foo.txt"), QLatin1String("9876543"));
    fileData1.fileOperation = FileData::ChangeFile;
    chunkData1.leftStartingLineNumber = 49;
    chunkData1.rightStartingLineNumber = 49;
    rows1.clear();
    rows1.append(RowData(TextLineData(QLatin1String("A"))));
    rows1.append(RowData(TextLineData(QLatin1String("B"))));
    rows1.append(RowData(TextLineData(QLatin1String("C"))));
    rows1.append(RowData(TextLineData(TextLineData::Separator),
                         TextLineData(QLatin1String(""))));
    chunkData1.rows = rows1;
    fileData1.chunks.clear();
    fileData1.chunks.append(chunkData1);

    QList<FileData> fileDataList2;
    fileDataList2 << fileData1;

    QTest::newRow("Added line") << patch
                                << fileDataList2;

    //////////////

    patch = QLatin1String("diff --git a/file foo.txt b/file foo.txt\n"
                          "index 1234567..9876543 100644\n"
                          "--- a/file foo.txt\n"
                          "+++ b/file foo.txt\n"
                          "@@ -1,1 +1,1 @@\n"
                          "-ABCD\n"
                          "\\ No newline at end of file\n"
                          "+ABCD\n");

    fileData1.leftFileInfo = DiffFileInfo(QLatin1String("file foo.txt"), QLatin1String("1234567"));
    fileData1.rightFileInfo = DiffFileInfo(QLatin1String("file foo.txt"), QLatin1String("9876543"));
    fileData1.fileOperation = FileData::ChangeFile;
    chunkData1.leftStartingLineNumber = 0;
    chunkData1.rightStartingLineNumber = 0;
    rows1.clear();
    rows1.append(RowData(TextLineData(QLatin1String("ABCD"))));
    rows1.append(RowData(TextLineData(TextLineData::Separator),
                         TextLineData(QLatin1String(""))));
    chunkData1.rows = rows1;
    fileData1.chunks.clear();
    fileData1.chunks.append(chunkData1);

    QList<FileData> fileDataList3;
    fileDataList3 << fileData1;

    QTest::newRow("Last newline added to a line without newline") << patch
                                << fileDataList3;
        QCOMPARE(resultFileData.fileOperation, origFileData.fileOperation);