USE [ECSB_SAP]
GO

/****** Object:  StoredProcedure [dbo].[IMApp_sp_GetDataList]    Script Date: 2024-05-10 12:13:12 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


-- =============================================
-- Author:		<Author,,Name>
-- Create date: <Create Date,,>
-- Description:	<Description,,>
-- =============================================
CREATE   PROCEDURE [dbo].[IMApp_sp_GetDataList] @Type VARCHAR(100), @Json NVARCHAR(MAX)
AS
BEGIN
	DECLARE @count INT
	DECLARE @query NVARCHAR(MAX)

	-- AP INVOICE
	IF @Type = 'APInvoices' AND @Json LIKE '%startDate%' AND @Json LIKE '%endDate%'
	BEGIN
		SELECT DocEntry, DocNum, DocDate, CardCode, CardName
		FROM OPCH
		WHERE DocStatus = 'O' AND DocDate >= JSON_VALUE(@Json, '$.startDate') AND DocDate <= JSON_VALUE(@Json, '$.endDate') AND DocType = 'I'
	END

	IF @Type = 'APInvoiceLines' AND @Json LIKE '%selected%'
	BEGIN
		SELECT ISNULL(LOWER(T4.Id), LOWER(NEWID())) AS Id, T0.DocEntry, T1.DocNum, T0.LineNum, T0.ItemCode, T0.Dscription, ISNULL(T0.unitMsr, T0.UomCode) AS Measurement, T0.NumPerMsr, T0.ObjType, 
			ISNULL(T4.WhsCode, T0.WhsCode) AS WhsCode, ISNULL(T4.BinActivat, T3.BinActivat) AS WhsBinActivat, T0.OpenCreQty AS OpenQty, T1.CardCode, T1.CardName, ISNULL(T4.Quantity, 0) AS ReceiptQty,
			T4.ShowList, ISNULL(T4.Allocations, '[]') AS Allocations, CASE WHEN T2.ManBtchNum = 'Y' THEN 'BATCH' WHEN T2.ManSerNum = 'Y' THEN 'SERIAL' ELSE 'NONE' END AS ManagedBy, CAST(CASE WHEN ISNULL(T4.Id, '') = '' THEN 0 ELSE 1 END AS BIT) AS IsDraft
		FROM PCH1 T0
		INNER JOIN OPCH T1 ON T1.DocEntry = T0.DocEntry
		INNER JOIN OITM T2 ON T2.ItemCode = T0.ItemCode
		INNER JOIN OWHS T3 ON T3.WhsCode = T0.WhsCode
		LEFT JOIN (
			SELECT I0.Id, I0.Quantity, I0.ShowList, I0.Allocations, I0.WhsCode, I0.LineNum, I1.UserId, I2.BinActivat, I0.BaseEntry
			FROM IMAppDocumentDraftLine I0
			INNER JOIN IMAppDocumentDraft I1 ON I1.Id = I0.DocGuid AND I1.[Status] = 'O' AND I1.ObjType = '234000032' AND I0.BaseType = 18
			INNER JOIN OWHS I2 ON I2.WhsCode = I0.WhsCode
			WHERE I0.BaseEntry IN (SELECT [value] FROM STRING_SPLIT(JSON_VALUE(@Json, '$.selected'), ',')) AND I0.LineStatus = 'O'
		) T4 ON T4.LineNum = T0.LineNum AND T4.UserId = JSON_VALUE(@Json, '$.userId') AND T4.BaseEntry = T0.DocEntry
		WHERE T0.DocEntry IN (SELECT [value] FROM STRING_SPLIT(JSON_VALUE(@Json, '$.selected'), ',')) AND T0.LineStatus = 'O'
		ORDER BY T0.DocEntry, T0.LineNum
	END

	-- AR INVOICE
	IF @Type = 'ARInvoices' AND @Json LIKE '%startDate%' AND @Json LIKE '%endDate%'
	BEGIN
		SELECT DocEntry, DocNum, DocDate, CardCode, CardName
		FROM OINV
		WHERE DocStatus = 'O' AND DocDate >= JSON_VALUE(@Json, '$.startDate') AND DocDate <= JSON_VALUE(@Json, '$.endDate') AND DocType = 'I'
	END

	IF @Type = 'ARInvoiceLines' AND @Json LIKE '%selected%'
	BEGIN
		SELECT ISNULL(LOWER(T4.Id), LOWER(NEWID())) AS Id, T0.DocEntry, T1.DocNum, T0.LineNum, T0.ItemCode, T0.Dscription, ISNULL(T0.unitMsr, T0.UomCode) AS Measurement, T0.NumPerMsr, T0.ObjType, 
			ISNULL(T4.WhsCode, T0.WhsCode) AS WhsCode, ISNULL(T4.BinActivat, T3.BinActivat) AS WhsBinActivat, T0.OpenCreQty AS OpenQty, T1.CardCode, T1.CardName, ISNULL(T4.Quantity, 0) AS ReceiptQty,
			T4.ShowList, ISNULL(T4.Allocations, '[]') AS Allocations, CASE WHEN T2.ManBtchNum = 'Y' THEN 'BATCH' WHEN T2.ManSerNum = 'Y' THEN 'SERIAL' ELSE 'NONE' END AS ManagedBy, CAST(CASE WHEN ISNULL(T4.Id, '') = '' THEN 0 ELSE 1 END AS BIT) AS IsDraft
		FROM INV1 T0
		INNER JOIN OINV T1 ON T1.DocEntry = T0.DocEntry
		INNER JOIN OITM T2 ON T2.ItemCode = T0.ItemCode
		INNER JOIN OWHS T3 ON T3.WhsCode = T0.WhsCode
		LEFT JOIN (
			SELECT I0.Id, I0.Quantity, I0.ShowList, I0.Allocations, I0.WhsCode, I0.LineNum, I1.UserId, I2.BinActivat, I0.BaseEntry
			FROM IMAppDocumentDraftLine I0
			INNER JOIN IMAppDocumentDraft I1 ON I1.Id = I0.DocGuid AND I1.[Status] = 'O' AND I0.BaseType = JSON_VALUE(@Json, '$.objType')
			INNER JOIN OWHS I2 ON I2.WhsCode = I0.WhsCode
			WHERE I0.BaseEntry IN (SELECT [value] FROM STRING_SPLIT(JSON_VALUE(@Json, '$.selected'), ',')) AND I0.LineStatus = 'O'
		) T4 ON T4.LineNum = T0.LineNum AND T4.UserId = JSON_VALUE(@Json, '$.userId') AND T4.BaseEntry = T0.DocEntry
		WHERE T0.DocEntry IN (SELECT [value] FROM STRING_SPLIT(JSON_VALUE(@Json, '$.selected'), ',')) AND T0.LineStatus = 'O'
		ORDER BY T0.DocEntry, T0.LineNum
	END

	-- BATCH NUMBER
	IF @Type = 'BatchInfo'
	BEGIN
		SELECT T0.DistNumber, T0.ExpDate, T0.MnfDate, T0.InDate, T0.MnfSerial, T0.LotNumber
		FROM OBTN T0
		WHERE T0.DistNumber = JSON_VALUE(@Json, '$.distNumber') AND T0.ItemCode = JSON_VALUE(@Json, '$.itemCode')
	END

	-- BUSINESS PARTNERS
	IF @Type = 'BusinessPartnerCount' AND @Json LIKE '%cardType%'
	BEGIN
		SET @query = 'SELECT COUNT(CardCode) AS TotalCount FROM OCRD WHERE CardType = ''' + JSON_VALUE(@Json, '$.cardType') + ''' '

		IF @Json LIKE '%search%'
		BEGIN
			SET @query += 'AND CardCode IN (SELECT I0.CardCode FROM OCRD I0 WHERE I0.CardCode + I0.CardName LIKE ''%' + JSON_VALUE(@json, '$.search') + '%'') '
		END

		EXEC (@query)
	END

	IF @Type = 'BusinessPartner' AND @Json LIKE '%type%'
	BEGIN
		SELECT CardCode, CardName
		FROM OCRD
		WHERE CardType = JSON_VALUE(@Json, '$.type')
	END

	-- COMPANY
	IF @Type = 'CompanyInfo'
	BEGIN
		SELECT T0.CompnyName, T1.WhsName, T0.DfltWhs
		FROM OADM T0
		INNER JOIN OWHS T1 ON T1.WhsCode = T0.DfltWhs
	END

	-- DELIVERY ORDER
	IF @Type = 'DeliveryOrders' AND @Json LIKE '%startDate%' AND @Json LIKE '%endDate%'
	BEGIN
		SELECT DocEntry, DocNum, DocDate, CardCode, CardName
		FROM ODLN
		WHERE DocStatus = 'O' AND DocDate >= JSON_VALUE(@Json, '$.startDate') AND DocDate <= JSON_VALUE(@Json, '$.endDate') AND DocType = 'I'
			AND DocEntry NOT IN (SELECT I0.DocEntry FROM DLN1 I0 GROUP BY I0.DocEntry HAVING COUNT(I0.DocEntry) = COUNT(CASE WHEN I0.LineStatus = 'C' THEN 1 ELSE NULL END))
	END

	IF @Type = 'DeliveryOrderLines' AND @Json LIKE '%selected%'
	BEGIN
		SELECT ISNULL(LOWER(T4.Id), LOWER(NEWID())) AS Id, T0.DocEntry, T1.DocNum, T0.LineNum, T0.ItemCode, T0.Dscription, ISNULL(T0.unitMsr, T0.UomCode) AS Measurement, T0.NumPerMsr, T0.ObjType, 
			ISNULL(T4.WhsCode, T0.WhsCode) AS WhsCode, ISNULL(T4.BinActivat, T3.BinActivat) AS WhsBinActivat, T0.OpenQty, T1.CardCode, T1.CardName, ISNULL(T4.Quantity, 0) AS ReceiptQty,
			T4.ShowList, ISNULL(T4.Allocations, '[]') AS Allocations, CASE WHEN T2.ManBtchNum = 'Y' THEN 'BATCH' WHEN T2.ManSerNum = 'Y' THEN 'SERIAL' ELSE 'NONE' END AS ManagedBy, CAST(CASE WHEN ISNULL(T4.Id, '') = '' THEN 0 ELSE 1 END AS BIT) AS IsDraft
		FROM DLN1 T0
		INNER JOIN ODLN T1 ON T1.DocEntry = T0.DocEntry
		INNER JOIN OITM T2 ON T2.ItemCode = T0.ItemCode
		INNER JOIN OWHS T3 ON T3.WhsCode = T0.WhsCode
		LEFT JOIN (
			SELECT I0.Id, I0.Quantity, I0.ShowList, I0.Allocations, I0.WhsCode, I0.LineNum, I1.UserId, I2.BinActivat, I0.BaseEntry
			FROM IMAppDocumentDraftLine I0
			INNER JOIN IMAppDocumentDraft I1 ON I1.Id = I0.DocGuid AND I1.[Status] = 'O' AND I0.BaseType = JSON_VALUE(@Json, '$.objType')
			INNER JOIN OWHS I2 ON I2.WhsCode = I0.WhsCode
			WHERE I0.BaseEntry IN (SELECT [value] FROM STRING_SPLIT(JSON_VALUE(@Json, '$.selected'), ',')) AND I0.LineStatus = 'O'
		) T4 ON T4.LineNum = T0.LineNum AND T4.UserId = JSON_VALUE(@Json, '$.userId') AND T4.BaseEntry = T0.DocEntry
		WHERE T0.DocEntry IN (SELECT [value] FROM STRING_SPLIT(JSON_VALUE(@Json, '$.selected'), ',')) AND T0.LineStatus = 'O'
		ORDER BY T0.DocEntry, T0.LineNum
	END

	-- DELIVERY ORDER BATCH / SERIAL
	IF @Type = 'DeliveryOrderBatch'
	BEGIN
		SET @query = 'SELECT T1.ItemCode, SUM(T2.Quantity) AS Quantity, T2.BatchNum AS DistNumber '
			+ 'FROM ODLN T0 LEFT JOIN DLN1 T1 ON T1.DocEntry = T0.DocEntry '
			+ 'LEFT JOIN IBT1 T2 ON T2.BaseEntry = T1.DocEntry AND T2.BaseLinNum = T1.LineNum AND T1.ObjType = T2.BaseType '
			+ 'LEFT JOIN OBTN T3 ON T3.DistNumber = T2.BatchNum AND T2.ItemCode = T3.ItemCode '

		 IF @Json LIKE '%itemCode%' AND @Json LIKE '%cardCode%'
		 BEGIN
			SET @query += 'WHERE T1.ItemCode = ''' + JSON_VALUE(@Json, '$.itemCode') + ''' AND T0.CardCode = ''' + JSON_VALUE(@Json, '$.cardCode') + ''' '
		 END
		 ELSE

		 IF @Json LIKE '%docEntry%' AND @Json LIKE '%lineNum%'
		 BEGIN
			SET @query += 'WHERE T0.DocEntry = ''' + JSON_VALUE(@Json, '$.docEntry') + ''' AND T1.LineNum = ''' + JSON_VALUE(@Json, '$.lineNum') + ''' AND T2.Direction = 1 '
		 END

		 SET @query += 'GROUP BY T1.ItemCode, T2.BatchNum, T2.Direction'

		 EXEC (@query)
	END

	IF @Type = 'DeliveryOrderSerials' AND @Json LIKE '%docEntry%' AND @Json LIKE '%lineNum%'
	BEGIN
		SELECT T2.IntrSerial AS DistNumber, T1.ItemCode
		FROM SRI1 T0
		INNER JOIN DLN1 T1 ON T0.BaseEntry = T1.DocEntry AND T0.BaseType = T1.ObjType AND T0.BaseLinNum = T1.LineNum
		INNER JOIN OSRI T2 ON T0.SysSerial = T2.SysSerial AND T0.ItemCode = T2.ItemCode
		INNER JOIN OSRN T3 ON T3.DistNumber = T2.IntrSerial
		WHERE T1.DocEntry = JSON_VALUE(@Json, '$.docEntry') AND T1.LineNum = JSON_VALUE(@Json, '$.lineNum') 
		GROUP BY T2.IntrSerial, T1.ItemCode
	END

	-- DOCUMENT DRAFT
	IF @Type = 'DraftHeader'
	BEGIN
		IF @Json LIKE '%documents%'
		BEGIN
			SELECT DISTINCT T0.*, T1.BaseEntry
			FROM IMAppDocumentDraft T0
			INNER JOIN IMAppDocumentDraftLine T1 ON T1.DocGuid = T0.Id
			WHERE T0.UserId = JSON_VALUE(@Json, '$.userId') AND T0.ObjType = JSON_VALUE(@Json, '$.objType') AND T0.[Status] = 'O' AND T1.BaseEntry IN (SELECT [value] FROM STRING_SPLIT(JSON_VALUE(@Json, '$.documents'), ',')) 

		END
		ELSE 
		BEGIN
			SELECT DISTINCT T0.*, -1 AS BaseEntry
			FROM IMAppDocumentDraft T0
			INNER JOIN IMAppDocumentDraftLine T1 ON T1.DocGuid = T0.Id
			WHERE T0.UserId = JSON_VALUE(@Json, '$.userId') AND T0.ObjType = JSON_VALUE(@Json, '$.objType') AND T1.BaseEntry = -1 AND T1.LineStatus = 'O' AND [Status] = 'O'
		END
	END

	IF @Type = 'DraftLines'
	BEGIN
		SELECT T0.Id, T0.BaseEntry AS DocEntry, -1 AS DocNum, T0.LineNum, T0.ItemCode, T1.ItemName AS Dscription, T2.ObjType, T0.WhsCode, T0.FromWhsCode, T3.BinActivat AS WhsBinActivat, T0.Quantity AS OpenQty,
			T2.CardCode, T2.CardName, T0.Quantity AS ReceiptQty, T0.ShowList, ISNULL(T0.Allocations, '[]') AS Allocations, CASE WHEN T1.ManBtchNum = 'Y' THEN 'BATCH' WHEN T1.ManSerNum = 'Y' THEN 'SERIAL' ELSE 'NONE' END AS ManagedBy, T5.UomCode AS Measurement, T4.BaseQty AS NumPerMsr,
			CAST(1 AS BIT) AS IsDraft
		FROM IMAppDocumentDraftLine T0
		INNER JOIN OITM T1 ON T1.ItemCode = T0.ItemCode
		INNER JOIN IMAppDocumentDraft T2 ON T2.Id = T0.DocGuid
		INNER JOIN OWHS T3 ON T3.WhsCode = T0.WhsCode
		INNER JOIN UGP1 T4 ON T4.UgpEntry = T1.UgpEntry
		INNER JOIN OUOM T5 ON T5.UomEntry = T4.UomEntry
		WHERE T0.UserId = JSON_VALUE(@Json, '$.userId') AND T0.BaseEntry = -1 AND LineStatus = 'O' AND T2.ObjType = JSON_VALUE(@Json, '$.objType') AND T2.[Status] = 'O'
		ORDER BY T0.LineNum
	END

	-- GOODS RECEIPT PO
	IF @Type = 'GRPO' AND @Json LIKE '%startDate%' AND @Json LIKE '%endDate%'
	BEGIN
		SELECT DocEntry, DocNum, DocDate, CardCode, CardName
		FROM OPDN
		WHERE DocStatus = 'O' AND DocDate >= JSON_VALUE(@Json, '$.startDate') AND DocDate <= JSON_VALUE(@Json, '$.endDate') AND DocType = 'I' 
			AND DocEntry NOT IN (SELECT I0.DocEntry FROM PDN1 I0 GROUP BY I0.DocEntry HAVING COUNT(I0.DocEntry) = COUNT(CASE WHEN I0.LineStatus = 'C' THEN 1 ELSE NULL END))
	END

	IF @Type = 'GRPOLines' AND @Json LIKE '%selected%'
	BEGIN
		SELECT ISNULL(LOWER(T4.Id), LOWER(NEWID())) AS Id, T0.DocEntry, T1.DocNum, T0.LineNum, T0.ItemCode, T0.Dscription, ISNULL(T0.unitMsr, T0.UomCode) AS Measurement, T0.NumPerMsr, T0.ObjType, 
			ISNULL(T4.WhsCode, T0.WhsCode) AS WhsCode, ISNULL(T4.BinActivat, T3.BinActivat) AS WhsBinActivat, T0.OpenCreQty AS OpenQty, T1.CardCode, T1.CardName, ISNULL(T4.Quantity, 0) AS ReceiptQty,
			T4.ShowList, ISNULL(T4.Allocations, '[]') AS Allocations, CASE WHEN T2.ManBtchNum = 'Y' THEN 'BATCH' WHEN T2.ManSerNum = 'Y' THEN 'SERIAL' ELSE 'NONE' END AS ManagedBy, CAST(CASE WHEN ISNULL(T4.Id, '') = '' THEN 0 ELSE 1 END AS BIT) AS IsDraft
		FROM PDN1 T0
		INNER JOIN OPDN T1 ON T1.DocEntry = T0.DocEntry
		INNER JOIN OITM T2 ON T2.ItemCode = T0.ItemCode
		INNER JOIN OWHS T3 ON T3.WhsCode = T0.WhsCode
		LEFT JOIN (
			SELECT I0.Id, I0.Quantity, I0.ShowList, I0.Allocations, I0.WhsCode, I0.LineNum, I1.UserId, I2.BinActivat, I0.BaseEntry
			FROM IMAppDocumentDraftLine I0
			INNER JOIN IMAppDocumentDraft I1 ON I1.Id = I0.DocGuid AND I1.[Status] = 'O' AND I0.BaseType = JSON_VALUE(@Json, '$.objType')
			INNER JOIN OWHS I2 ON I2.WhsCode = I0.WhsCode
			WHERE I0.BaseEntry IN (SELECT [value] FROM STRING_SPLIT(JSON_VALUE(@Json, '$.selected'), ',')) AND I0.LineStatus = 'O'
		) T4 ON T4.LineNum = T0.LineNum AND T4.UserId = JSON_VALUE(@Json, '$.userId') AND T4.BaseEntry = T0.DocEntry
		WHERE T0.DocEntry IN (SELECT [value] FROM STRING_SPLIT(JSON_VALUE(@Json, '$.selected'), ',')) AND T0.LineStatus = 'O'
		ORDER BY T0.DocEntry, T0.LineNum
	END

	IF @Type = 'GRPOSnB' AND @Json LIKE '%docEntry%'
	BEGIN
		SELECT T0.DocEntry, T1.ItemCode, CASE T2.ManagedBy WHEN 10000044 THEN 'Batch' ELSE 'Serial' END AS ManagedBy, CASE T2.ManagedBy WHEN 10000045 THEN T5.DistNumber WHEN 10000044 THEN T4.DistNumber END AS DistNumber
		FROM OPDN T0
		INNER JOIN PDN1 T1 ON T1.DocEntry = T0.DocEntry
		INNER JOIN OITL T2 ON T1.DocEntry = T2.ApplyEntry AND T1.LineNum = T2.ApplyLine AND T2.ApplyType = 20
		INNER JOIN ITL1 T3 ON T2.LogEntry = T3.LogEntry 
		LEFT JOIN OBTN T4 ON T4.[ItemCode] = T3.[ItemCode] AND T3.[MdAbsEntry] = T4.AbsEntry
		LEFT JOIN OSRN T5 ON T5.ItemCode = T1.ItemCode AND T5.AbsEntry = T3.MdAbsEntry
		WHERE T0.DocEntry = JSON_VALUE(@Json, '$.docEntry')
	END

	-- GOODS RETURN REQUEST
	IF @Type = 'GoodsReturnRequests' AND @Json LIKE '%startDate%' AND @Json LIKE '%endDate%'
	BEGIN
		SELECT DocEntry, DocNum, DocDate, CardCode, CardName
		FROM OPRR
		WHERE DocStatus = 'O' AND DocDate >= JSON_VALUE(@Json, '$.startDate') AND DocDate <= JSON_VALUE(@Json, '$.endDate') AND DocType = 'I'
	END

	IF @Type = 'GoodsReturnRequestLines' AND @Json LIKE '%selected%'
	BEGIN
		SELECT ISNULL(LOWER(T4.Id), LOWER(NEWID())) AS Id, T0.DocEntry, T1.DocNum, T0.LineNum, T0.ItemCode, T0.Dscription, ISNULL(T0.unitMsr, T0.UomCode) AS Measurement, T0.NumPerMsr, T0.ObjType, 
			ISNULL(T4.WhsCode, T0.WhsCode) AS WhsCode, ISNULL(T4.BinActivat, T3.BinActivat) AS WhsBinActivat, T0.OpenCreQty AS OpenQty, T1.CardCode, T1.CardName, ISNULL(T4.Quantity, 0) AS ReceiptQty,
			T4.ShowList, ISNULL(T4.Allocations, '[]') AS Allocations, CASE WHEN T2.ManBtchNum = 'Y' THEN 'BATCH' WHEN T2.ManSerNum = 'Y' THEN 'SERIAL' ELSE 'NONE' END AS ManagedBy, CAST(CASE WHEN ISNULL(T4.Id, '') = '' THEN 0 ELSE 1 END AS BIT) AS IsDraft
		FROM PRR1 T0
		INNER JOIN OPRR T1 ON T1.DocEntry = T0.DocEntry
		INNER JOIN OITM T2 ON T2.ItemCode = T0.ItemCode
		INNER JOIN OWHS T3 ON T3.WhsCode = T0.WhsCode
		LEFT JOIN (
			SELECT I0.Id, I0.Quantity, I0.ShowList, I0.Allocations, I0.WhsCode, I0.LineNum, I1.UserId, I2.BinActivat, I0.BaseEntry
			FROM IMAppDocumentDraftLine I0
			INNER JOIN IMAppDocumentDraft I1 ON I1.Id = I0.DocGuid AND I1.[Status] = 'O' AND I0.BaseType = JSON_VALUE(@Json, '$.objType')
			INNER JOIN OWHS I2 ON I2.WhsCode = I0.WhsCode
			WHERE I0.BaseEntry IN (SELECT [value] FROM STRING_SPLIT(JSON_VALUE(@Json, '$.selected'), ',')) AND I0.LineStatus = 'O'
		) T4 ON T4.LineNum = T0.LineNum AND T4.UserId = JSON_VALUE(@Json, '$.userId') AND T4.BaseEntry = T0.DocEntry
		WHERE T0.DocEntry IN (SELECT [value] FROM STRING_SPLIT(JSON_VALUE(@Json, '$.selected'), ',')) AND T0.LineStatus = 'O'
		ORDER BY T0.DocEntry, T0.LineNum
	END

	-- INVENTORY COUNTING
	IF @Type = 'InventoryCounting' AND @Json LIKE '%startDate%' AND @Json LIKE '%endDate%'
	BEGIN
		SELECT T0.DocEntry, T0.DocNum, T0.CreateDate AS DocDate
		FROM OINC T0
		WHERE [Status] = 'O' AND CreateDate >= JSON_VALUE(@Json, '$.startDate') AND CreateDate <= JSON_VALUE(@Json, '$.endDate')
	END

	IF @Type = 'InventoryCountingLines' AND @Json LIKE '%docEntry%'
	BEGIN
		SELECT ISNULL(LOWER(T4.Id), LOWER(NEWID())) AS Id, T0.DocEntry, T0.LineNum, T0.ItemCode, T0.ItemDesc AS Dscription, T0.UomCode AS Measurement, T0.WhsCode, ISNULL(T0.BinEntry, 0) AS BinEntry, 
			T1.BinCode, COALESCE(T4.CountQty, T0.CountQty) AS CountQty, COALESCE(T4.Counted, T0.Counted) AS Counted, T4.ShowList, ISNULL(T4.Allocations, '[]') AS Allocations,
			CASE WHEN T2.ManBtchNum = 'Y' THEN 'BATCH' WHEN T2.ManSerNum = 'Y' THEN 'SERIAL' ELSE 'NONE' END AS ManagedBy, T3.BinActivat AS WhsBinActivat,
			CAST(CASE WHEN ISNULL(T4.Id, '') = '' THEN 0 ELSE 1 END AS BIT) AS IsDraft, T4.DraftId
		FROM INC1 T0
		LEFT JOIN OBIN T1 ON T1.AbsEntry = T0.BinEntry
		LEFT JOIN OITM T2 ON T2.ItemCode = T0.ItemCode
		LEFT JOIN OWHS T3 ON T3.WhsCode = T0.WhsCode
		LEFT JOIN (
			SELECT I0.Id, I0.CountQty, I0.ShowList, I0.Allocations, I0.LineNum, I1.UserId, I0.Counted, I1.Id AS DraftId
			FROM IMAppInventoryCountingDraftLine I0
			INNER JOIN IMAppInventoryCountingDraft I1 ON I1.Id = I0.DocGuid AND I1.[Status] = 'O'
			WHERE I1.DocEntry = JSON_VALUE(@Json, '$.docEntry') AND I1.UserId = JSON_VALUE(@Json, '$.userId')
		) T4 ON T4.LineNum = T0.LineNum 
		WHERE T0.DocEntry = JSON_VALUE(@Json, '$.docEntry')

		UNION ALL

		SELECT I0.Id, I1.DocEntry, I0.LineNum, I0.ItemCode, T2.ItemName AS Dscription, ISNULL(T2.InvntryUom, 'Manual') AS Measurement, I0.WhsCode, ISNULL(I0.BinEntry, 0) AS BinEntry,
			T3.BinCode, I0.CountQty, I0.Counted, I0.ShowList, ISNULL(I0.Allocations, '[]') AS Allocations, 
			CASE WHEN T2.ManBtchNum = 'Y' THEN 'BATCH' WHEN T2.ManSerNum = 'Y' THEN 'SERIAL' ELSE 'NONE' END AS ManagedBy, T4.BinActivat AS WhsBinActivat, CAST(1 AS BIT) AS IsDraft, I0.DocGuid AS DraftId
		FROM IMAppInventoryCountingDraftLine I0
		INNER JOIN IMAppInventoryCountingDraft I1 ON I1.Id = I0.DocGuid AND I1.[Status] = 'O'
		LEFT JOIN OITM T2 ON T2.ItemCode = I0.ItemCode
		LEFT JOIN OBIN T3 ON T3.AbsEntry = I0.BinEntry
		LEFT JOIN OWHS T4 ON T4.WhsCode = I0.WhsCode
		WHERE I1.DocEntry = JSON_VALUE(@Json, '$.docEntry') AND I1.UserId = JSON_VALUE(@Json, '$.userId') AND I0.LineNum = -1
	END

	IF @Type = 'InventoryCountingLineSnB' AND @Json LIKE '%docEntry%'
	BEGIN
		SELECT T0.LineNum, T0.ItemCode, T0.ObjAbs, 
			CASE T0.[ObjId] WHEN 10000045 THEN T1.DistNumber WHEN 10000044 THEN T2.DistNumber WHEN 10000068 THEN T3.DistNumber WHEN 10000069 THEN T4.DistNumber END AS DistNumber,
			T0.Quantity
		FROM INC3 T0
		LEFT JOIN OSRN T1 ON T1.AbsEntry = T0.ObjAbs
		LEFT JOIN OBTN T2 ON T2.AbsEntry = T0.ObjAbs
		LEFT JOIN ODBN T3 ON T3.AbsEntry = T0.ObjAbs
		LEFT JOIN ODSN T4 ON T4.AbsEntry = T0.ObjAbs
		WHERE T0.DocEntry = JSON_VALUE(@Json, '$.docEntry')
	END

	IF @Type = 'InventoryCountingNewLine' AND @Json LIKE '%itemCode%' AND @Json LIKE '%whsCode%' AND @Json LIKE '%binEntry%'
	BEGIN
		SELECT T0.DocEntry, T0.DocNum
		FROM OINC T0
		INNER JOIN INC1 T1 ON T1.DocEntry = T0.DocEntry
		WHERE T0.[Status] = 'O' AND T1.ItemCode = JSON_VALUE(@Json, '$.itemCode') AND T1.WhsCode = JSON_VALUE(@Json, '$.whsCode') AND COALESCE(T1.BinEntry, '') = COALESCE(JSON_VALUE(@Json, '$.binEntry'), '')
	END

	IF @Type = 'InventoryCountingDraftLine'
	BEGIN
		SELECT T0.Id, T1.DocEntry, T0.LineNum, T0.ItemCode, T2.ItemName AS Dscription, ISNULL(T2.InvntryUom, 'Manual') AS Measurement, T0.DocGuid AS DraftId, T0.WhsCode, ISNULL(T0.BinEntry, 0) AS BinEntry,
			T3.BinCode, T0.CountQty, T0.Counted, T0.ShowList, ISNULL(T0.Allocations, '[]') AS Allocations, CASE WHEN T2.ManBtchNum = 'Y' THEN 'BATCH' WHEN T2.ManSerNum = 'Y' THEN 'SERIAL' ELSE 'NONE' END AS ManagedBy,
			T4.BinActivat AS WhsBinActivat, CAST(1 AS BIT) AS IsDraft
		FROM IMAppInventoryCountingDraftLine T0
		INNER JOIN IMAppInventoryCountingDraft T1 ON T1.Id = T0.DocGuid
		INNER JOIN OITM T2 ON T2.ItemCode = T0.ItemCode
		LEFT JOIN OBIN T3 ON T3.AbsEntry = T0.BinEntry
		INNER JOIN OWHS T4 ON T4.WhsCode = T0.WhsCode
		WHERE T1.DocEntry = -1 AND T1.UserId = JSON_VALUE(@Json, '$.userId') AND T1.[Status] = 'O'
	END

	-- ITEM MASTER
	IF @Type = 'ItemMasterCount'
	BEGIN
		SET @query = 'SELECT COUNT(ItemCode) AS TotalCount FROM OITM WHERE InvntItem = ''Y'' '

		IF @Json LIKE '%searchText%'
		BEGIN
			SET @query += 'AND (ItemCode + ItemName) LIKE ''%' + JSON_VALUE(@Json, '$.searchText') + '%'' '
		END

		EXEC (@query)
	END

	IF @Type = 'ItemMaster'
	BEGIN
		SET @query = 'SELECT ItemName, ItemCode FROM OITM WHERE InvntItem = ''Y'' '

		IF @Json LIKE '%searchText%'
		BEGIN
			SET @query += 'AND (ItemCode + ItemName) LIKE ''%' + JSON_VALUE(@Json, '$.searchText') + '%'' '
		END

		IF @Json LIKE '%page%' AND @Json LIKE '%size%'
		BEGIN
			SET @count = (CAST(JSON_VALUE(@json, '$.page') AS INT) - 1) * CAST(JSON_VALUE(@json, '$.size') AS INT)
			SET @query += 'ORDER BY ItemCode OFFSET ' + CAST(@count AS NVARCHAR) + ' ROWS FETCH NEXT ' + JSON_VALUE(@json, '$.size') + ' ROWS ONLY'
		END

		EXEC (@query)
	END

	IF @Type = 'ItemCode' AND @Json LIKE '%itemCode%'
	BEGIN
		SELECT T0.ItemCode, T0.ItemName, T0.ManBtchNum, T0.ManSerNum, ISNULL(T0.InvntryUom, T2.UomCode) AS UomCode, ISNULL(T0.IUoMEntry, T2.UomEntry) AS UomEntry, T2.UomName, T0.BuyUnitMsr, T0.NumInBuy, 
			T0.NumInSale, T0.SalUnitMsr, T0.InvntItem
		FROM OITM T0
		INNER JOIN OBCD T1 ON T1.ItemCode = T0.ItemCode
		INNER JOIN OUOM T2 ON T2.UomEntry = T1.UomEntry
		WHERE T0.ItemCode = JSON_VALUE(@Json, '$.itemCode')
	END

	IF @Type = 'ItemBarcode' AND @Json LIKE '%barcode%'
	BEGIN
		IF @Json LIKE '%whsCode%'
		BEGIN
			SELECT T0.ItemCode, T0.ItemName AS Dscription, ISNULL(T0.InvntryUom, T2.UomCode) AS Measurement, ISNULL(T0.IUoMEntry, T2.UomEntry) AS UomEntry, 
				T0.BuyUnitMsr, T0.NumInBuy, T0.NumInSale, T0.SalUnitMsr, T3.WhsCode, T3.BinActivat AS WhsBinActivat, 
				CASE WHEN T0.ManBtchNum = 'Y' THEN 'BATCH' WHEN T0.ManSerNum = 'Y' THEN 'SERIAL' ELSE 'NONE' END AS ManagedBy
			FROM OITM T0
			LEFT JOIN OBCD T1 ON T1.ItemCode = T0.ItemCode
			LEFT JOIN OUOM T2 ON T2.UomEntry = T1.UomEntry OR T2.UomEntry = T0.IUoMEntry
			LEFT JOIN OWHS T3 ON T3.WhsCode = JSON_VALUE(@Json, '$.whsCode')
			WHERE T0.ItemCode = JSON_VALUE(@Json, '$.barcode')
		END
		ELSE
		BEGIN
			SELECT T0.ItemCode, T0.ItemName AS Dscription, T0.ManBtchNum, T0.ManSerNum, ISNULL(T0.InvntryUom, T2.UomCode) AS Measurement, ISNULL(T0.IUoMEntry, T2.UomEntry) AS UomEntry,
				T0.BuyUnitMsr, T0.NumInBuy, T0.NumInSale, T0.SalUnitMsr, CASE WHEN T0.ManBtchNum = 'Y' THEN 'BATCH' WHEN T0.ManSerNum = 'Y' THEN 'SERIAL' ELSE 'NONE' END AS ManagedBy
			FROM OITM T0
			LEFT JOIN OBCD T1 ON T1.ItemCode = T0.ItemCode
			LEFT JOIN OUOM T2 ON T2.UomEntry = T1.UomEntry
			WHERE T0.ItemCode = JSON_VALUE(@Json, '$.barcode')
		END
	END

	IF @Type = 'ItemStockBalance' AND @Json LIKE '%itemCode%' AND @Json LIKE '%type%'
	BEGIN
		DECLARE @itemtype NVARCHAR(30)
		SET @itemtype = JSON_VALUE(@Json, '$.type')

		IF @itemtype = 'NonManagedItem'
		BEGIN
			SELECT T0.ItemCode, T0.ItemName, T1.WhsCode, T2.WhsName, T1.OnHand, T1.IsCommited, T1.OnOrder, (T1.OnHand + T1.OnOrder) - T1.IsCommited AS Available
			FROM OITM T0
			LEFT JOIN OITW T1 ON T1.ItemCode = T0.ItemCode
			LEFT JOIN OWHS T2 ON T2.WhsCode = T1.WhsCode
			WHERE T0.ItemCode = JSON_VALUE(@Json, '$.itemCode') AND (T1.OnHand > 0 OR T1.IsCommited > 0 OR T1.OnOrder > 0)
			ORDER BY T1.WhsCode
		END
		ELSE IF @itemtype = 'NonManagedItemWithBin'
		BEGIN
			SELECT T0.ItemCode, T0.ItemName, T1.WhsCode, T3.WhsName, T2.BinCode, T1.OnHandQty AS OnHand
			FROM OITM T0
			LEFT JOIN OIBQ T1 ON T1.ItemCode = T0.ItemCode
			LEFT JOIN OBIN T2 ON T2.AbsEntry = T1.BinAbs
			LEFT JOIN OWHS T3 ON T3.WhsCode = T1.WhsCode
			WHERE T0.ItemCode = JSON_VALUE(@Json, '$.itemCode') AND T1.OnHandQty > 0
			ORDER BY T2.BinCode
		END
		ELSE IF @itemtype = 'Serial'
		BEGIN
			SELECT T0.ItemCode, T0.ItemName, T1.WhsCode, T2.WhsName, T3.DistNumber
			FROM OITM T0
			LEFT JOIN OSRQ T1 ON T1.ItemCode = T0.ItemCode
			LEFT JOIN OWHS T2 ON T2.WhsCode = T1.WhsCode
			LEFT JOIN OSRN T3 ON T3.AbsEntry = T1.MdAbsEntry
			WHERE T0.ItemCode = JSON_VALUE(@Json, '$.itemCode') AND (T1.Quantity > 0 OR T1.CommitQty > 0 OR T1.CountQty > 0)
			ORDER BY T1.WhsCode, T3.DistNumber
		END
		ELSE IF @itemtype = 'SerialWithBin'
		BEGIN
			SELECT T0.ItemCode, T0.ItemName, T1.WhsCode, T2.WhsName, T4.DistNumber
			FROM OITM T0
			LEFT JOIN OSBQ T1 ON T1.ItemCode = T0.ItemCode
			LEFT JOIN OWHS T2 ON T2.WhsCode = T1.WhsCode
			LEFT JOIN OBIN T3 ON T3.AbsEntry = T1.BinAbs
			LEFT JOIN OSRN T4 ON T4.AbsEntry = T1.SnBMDAbs AND T0.ItemCode = T4.ItemCode
			WHERE T0.ItemCode = JSON_VALUE(@Json, '$.itemCode') AND T1.OnHandQty > 0
			ORDER BY T1.WhsCode, T3.BinCode, T4.DistNumber
		END
		ELSE IF @itemtype = 'Batch'
		BEGIN
			SELECT T0.ItemCode, T0.ItemName, T1.WhsCode, T3.WhsName, T2.DistNumber, T1.Quantity AS OnHand, T1.CommitQty AS IsCommited, T1.CountQty AS OnOrder, T2.ExpDate
			FROM OITM T0
			LEFT JOIN OBTQ T1 ON T1.ItemCode = T0.ItemCode
			LEFT JOIN OBTN T2 ON T2.AbsEntry = T1.MdAbsEntry
			LEFT JOIN OWHS T3 ON T3.WhsCode = T1.WhsCode
			WHERE T0.ItemCode = JSON_VALUE(@Json, '$.itemCode') AND (T1.Quantity > 0 OR T1.CommitQty > 0 OR T1.CountQty > 0)
			ORDER BY T1.WhsCode, T2.DistNumber
		END
		ELSE IF @itemtype = 'BatchWithBin'
		BEGIN
			SELECT T0.ItemCode, T0.ItemName, T1.WhsCode, T3.WhsName, T4.DistNumber, T2.BinCode, T1.OnHandQty AS OnHand, T4.ExpDate
			FROM OITM T0
			LEFT JOIN OBBQ T1 ON T1.ItemCode = T0.ItemCode
			LEFT JOIN OBIN T2 ON T2.AbsEntry = T1.BinAbs
			LEFT JOIN OWHS T3 ON T3.WhsCode = T1.WhsCode
			INNER JOIN OBTN T4 ON T4.AbsEntry = T1.SnBMDAbs AND T0.ItemCode = T4.ItemCode
			WHERE T0.ItemCode = JSON_VALUE(@Json, '$.itemCode') AND T1.OnHandQty > 0
			ORDER BY T1.WhsCode, T2.BinCode, T4.DistNumber
		END
	END

	-- ITEM WAREHOUSE BATCH / SERIAL
	IF @Type = 'WarehouseBatch'
	BEGIN
		IF @Json LIKE '%batchNumber%'
		BEGIN
			SELECT T0.ItemCode, T0.WhsCode, T1.DistNumber, T1.InDate, T0.AbsEntry, T0.Quantity AS OnHandQty 
			FROM OBTQ T0 
			LEFT OUTER JOIN OBTN T1 ON T1.AbsEntry = T0.MdAbsEntry
			WHERE T0.ItemCode = JSON_VALUE(@Json, '$.itemCode') AND T0.WhsCode = JSON_VALUE(@Json, '$.whsCode') AND T0.Quantity > 0 AND T1.DistNumber = JSON_VALUE(@Json, '$.batchNumber')
		END
		ELSE IF @Json LIKE '%docEntry%' AND @Json LIKE '%lineNum%'
		BEGIN
			WITH CTE AS (
				SELECT T0.ObjType, MAX(T1.LogEntry) AS [MaxLogEntry], T1.ItemCode, T1.DocLine AS LineNum, T2.SysNumber, T1.ManagedBy, T0.WhsCode
				FROM RDR1 T0
				INNER JOIN OITL T1 WITH (NOLOCK) ON T1.DocEntry = T0.DocEntry AND T1.DocType = T0.ObjType AND T1.DocLine = T0.LineNum
				INNER JOIN ITL1 T2 WITH (NOLOCK) ON T2.LogEntry = T1.LogEntry
				WHERE T0.DocEntry = JSON_VALUE(@Json, '$.docEntry') AND T0.LineNum = JSON_VALUE(@Json, '$.lineNum')
				GROUP BY T0.ObjType, T1.ItemCode, T1.DocLine, T2.SysNumber, T1.ManagedBy, T0.WhsCode
			), CTE_ALLOCATED AS (
				SELECT T0.WhsCode, CASE T0.ManagedBy WHEN 10000044 THEN T2.DistNumber WHEN 10000045 THEN T1.IntrSerial END AS DistNumber, T0.SysNumber AS AbsEntry,
					CASE WHEN V0.AllocQty > 0 THEN V0.AllocQty ELSE CASE WHEN V0.Quantity < 0 THEN V0.Quantity * -1 ELSE V0.Quantity END END AS OnHandQty
				FROM CTE T0
				CROSS APPLY (
					SELECT I0.OrderedQty, I0.AllocQty, I0.Quantity
					FROM ITL1 I0
					WHERE I0.LogEntry = T0.MaxLogEntry AND I0.ItemCode = T0.ItemCode AND I0.SysNumber = T0.SysNumber AND (I0.AllocQty > 0 OR I0.Quantity <> 0)
				) V0
				LEFT JOIN OSRI T1 ON T1.SysSerial = T0.SysNumber AND T1.ItemCode = T0.ItemCode AND T0.ManagedBy = 10000045
				LEFT JOIN OBTN T2 ON T2.SysNumber = T0.SysNumber AND T2.ItemCode = T0.ItemCode AND T0.ManagedBy = 10000044
			), CTE_AVAILABLE AS (
				SELECT T1.WhsCode, T0.[BatchNum] AS DistNumber, SUM(CASE T0.[Direction] WHEN 0 THEN 1 ELSE -1 END * T0.[Quantity]) + ISNULL(T3.OnHandQty, 0) AS OnHandQty, T2.AbsEntry, ISNULL(T3.OnHandQty, 0) AS AssignedQty
				FROM IBT1 T0 
				INNER JOIN OWHS T1 ON T0.WhsCode = T1.WhsCode
				INNER JOIN OBTN T2 ON T2.DistNumber = T0.BatchNum
				INNER JOIN (
					SELECT T0.[ItemCode], T1.WhsCode
					FROM IBT1 T0 
					INNER JOIN OWHS T1 ON T0.WhsCode = T1.WhsCode
					GROUP BY T1.WhsCode, T0.[ItemCode]
				) V0 ON T0.[ItemCode] = V0.[ItemCode] and T1.WhsCode = V0.WhsCode
				LEFT JOIN CTE_ALLOCATED T3 ON T3.AbsEntry = T2.AbsEntry
				WHERE T0.[ItemCode] = JSON_VALUE(@Json, '$.itemCode') AND T0.WhsCode = JSON_VALUE(@Json, '$.whsCode')
				GROUP BY T0.[BatchNum], T1.WhsCode, T0.[ItemCode], T2.AbsEntry, T3.OnHandQty
				HAVING SUM(CASE T0.[Direction] WHEN 0 THEN 1 ELSE -1 END * T0.[Quantity]) > 0
			)

			SELECT T0.*
			FROM (
				SELECT WhsCode, DistNumber, OnHandQty, AbsEntry, AssignedQty
				FROM CTE_AVAILABLE
				UNION
				SELECT WhsCode, DistNumber, OnHandQty, AbsEntry, OnHandQty AS AssignedQty
				FROM CTE_ALLOCATED 
				WHERE AbsEntry NOT IN (SELECT AbsEntry FROM CTE_AVAILABLE)
			) T0
			ORDER BY T0.AbsEntry 
		END
		ELSE
		BEGIN
			SELECT T1.WhsCode, T0.[BatchNum] AS DistNumber, SUM(CASE T0.[Direction] WHEN 0 THEN 1 ELSE -1 END * T0.[Quantity]) AS OnHandQty, T2.AbsEntry
			FROM IBT1 T0 
			INNER JOIN OWHS T1 ON T0.WhsCode = T1.WhsCode
			INNER JOIN OBTN T2 ON T2.DistNumber = T0.BatchNum
			INNER JOIN (
				SELECT T0.[ItemCode], T1.WhsCode
				FROM IBT1 T0 
				INNER JOIN OWHS T1 ON T0.WhsCode = T1.WhsCode
				GROUP BY T1.WhsCode, T0.[ItemCode]
			) V0 ON T0.[ItemCode] = V0.[ItemCode] and T1.WhsCode = V0.WhsCode 
			WHERE T0.[ItemCode] = JSON_VALUE(@Json, '$.itemCode') AND T0.WhsCode = JSON_VALUE(@Json, '$.whsCode')
			GROUP BY T0.[BatchNum], T1.WhsCode, T0.[ItemCode], T2.AbsEntry
			HAVING SUM(CASE T0.[Direction] WHEN 0 THEN 1 ELSE -1 END * T0.[Quantity]) > 0
			ORDER BY T2.AbsEntry
		END
	END

	IF @Type = 'SOBatchAllocation'
	BEGIN
		WITH CTE AS (
			SELECT T0.ObjType, MAX(T1.LogEntry) AS [MaxLogEntry], T1.ItemCode, T1.DocLine AS LineNum, T2.SysNumber, T1.ManagedBy, T0.WhsCode
			FROM RDR1 T0
			INNER JOIN OITL T1 WITH (NOLOCK) ON T1.DocEntry = T0.DocEntry AND T1.DocType = T0.ObjType AND T1.DocLine = T0.LineNum
			INNER JOIN ITL1 T2 WITH (NOLOCK) ON T2.LogEntry = T1.LogEntry
			WHERE T0.DocEntry = JSON_VALUE(@Json, '$.docEntry') AND T0.LineNum = JSON_VALUE(@Json, '$.lineNum')
			GROUP BY T0.ObjType, T1.ItemCode, T1.DocLine, T2.SysNumber, T1.ManagedBy, T0.WhsCode
		)
		
		SELECT T0.WhsCode, CASE T0.ManagedBy WHEN 10000044 THEN T2.DistNumber WHEN 10000045 THEN T1.IntrSerial END AS DistNumber, T0.SysNumber AS AbsEntry,
				CASE WHEN V0.AllocQty > 0 THEN V0.AllocQty ELSE CASE WHEN V0.Quantity < 0 THEN V0.Quantity * -1 ELSE V0.Quantity END END AS OnHandQty
		FROM CTE T0
		CROSS APPLY (
			SELECT I0.OrderedQty, I0.AllocQty, I0.Quantity
			FROM ITL1 I0
			WHERE I0.LogEntry = T0.MaxLogEntry AND I0.ItemCode = T0.ItemCode AND I0.SysNumber = T0.SysNumber AND (I0.AllocQty > 0 OR I0.Quantity <> 0)
		) V0
		LEFT JOIN OSRI T1 ON T1.SysSerial = T0.SysNumber AND T1.ItemCode = T0.ItemCode AND T0.ManagedBy = 10000045
		LEFT JOIN OBTN T2 ON T2.SysNumber = T0.SysNumber AND T2.ItemCode = T0.ItemCode AND T0.ManagedBy = 10000044
	END

	IF @Type = 'SOSerialAllocation'
	BEGIN
		WITH CTE AS (
			SELECT T0.ObjType, MAX(T1.LogEntry) AS [MaxLogEntry], T1.ItemCode, T1.DocLine AS LineNum, T2.SysNumber, T1.ManagedBy, T0.WhsCode
			FROM RDR1 T0
			INNER JOIN OITL T1 WITH (NOLOCK) ON T1.DocEntry = T0.DocEntry AND T1.DocType = T0.ObjType AND T1.DocLine = T0.LineNum
			INNER JOIN ITL1 T2 WITH (NOLOCK) ON T2.LogEntry = T1.LogEntry
			WHERE T0.DocEntry = JSON_VALUE(@Json, '$.docEntry') AND T0.LineNum = JSON_VALUE(@Json, '$.lineNum')
			GROUP BY T0.ObjType, T1.ItemCode, T1.DocLine, T2.SysNumber, T1.ManagedBy, T0.WhsCode
		)
		
		SELECT T0.WhsCode, T1.IntrSerial AS DistNumber, T0.SysNumber AS AbsEntry, CASE WHEN V0.AllocQty > 0 THEN V0.AllocQty ELSE CASE WHEN V0.Quantity < 0 THEN V0.Quantity * -1 ELSE V0.Quantity END END AS AssignedQty
		FROM CTE T0
		CROSS APPLY (
			SELECT I0.OrderedQty, I0.AllocQty, I0.Quantity
			FROM ITL1 I0
			WHERE I0.LogEntry = T0.MaxLogEntry AND I0.ItemCode = T0.ItemCode AND I0.SysNumber = T0.SysNumber AND (I0.AllocQty > 0 OR I0.Quantity <> 0)
		) V0
		LEFT JOIN OSRI T1 ON T1.SysSerial = T0.SysNumber AND T1.ItemCode = T0.ItemCode AND T0.ManagedBy = 10000045
	END

	IF @Type = 'WarehouseSerials'
	BEGIN
		IF @Json LIKE '%docEntry%' AND @Json LIKE '%lineNum%'
		BEGIN
			WITH CTE AS (
				SELECT T0.ObjType, MAX(T1.LogEntry) AS [MaxLogEntry], T1.ItemCode, T1.DocLine AS LineNum, T2.SysNumber, T1.ManagedBy, T0.WhsCode
				FROM RDR1 T0
				INNER JOIN OITL T1 WITH (NOLOCK) ON T1.DocEntry = T0.DocEntry AND T1.DocType = T0.ObjType AND T1.DocLine = T0.LineNum
				INNER JOIN ITL1 T2 WITH (NOLOCK) ON T2.LogEntry = T1.LogEntry
				WHERE T0.DocEntry = JSON_VALUE(@Json, '$.docEntry') AND T0.LineNum = JSON_VALUE(@Json, '$.lineNum')
				GROUP BY T0.ObjType, T1.ItemCode, T1.DocLine, T2.SysNumber, T1.ManagedBy, T0.WhsCode
			), CTE_ALLOCATED AS (
				SELECT T0.WhsCode, T1.IntrSerial AS DistNumber, T0.SysNumber AS AbsEntry, CASE WHEN V0.AllocQty > 0 THEN V0.AllocQty ELSE CASE WHEN V0.Quantity < 0 THEN V0.Quantity * -1 ELSE V0.Quantity END END AS OnHandQty
				FROM CTE T0
				CROSS APPLY (
					SELECT I0.OrderedQty, I0.AllocQty, I0.Quantity
					FROM ITL1 I0
					WHERE I0.LogEntry = T0.MaxLogEntry AND I0.ItemCode = T0.ItemCode AND I0.SysNumber = T0.SysNumber AND (I0.AllocQty > 0 OR I0.Quantity <> 0)
				) V0
				LEFT JOIN OSRI T1 ON T1.SysSerial = T0.SysNumber AND T1.ItemCode = T0.ItemCode AND T0.ManagedBy = 10000045
			), CTE_AVAILABLE AS (
				SELECT T1.SysNumber AS AbsEntry, T0.ItemCode, T1.DistNumber, T0.WhsCode, 0 AS AssignedQty
				FROM OSRQ T0
				LEFT JOIN OSRN T1 ON T1.ItemCode = T0.ItemCode AND T0.MdAbsEntry = T1.AbsEntry 
				WHERE T0.ItemCode = JSON_VALUE(@Json, '$.itemCode') AND T0.WhsCode = JSON_VALUE(@Json, '$.whsCode') AND (T0.Quantity > 0 OR T0.CommitQty > 0 OR T0.CountQty > 0) 
					AND T1.DistNumber NOT IN (SELECT DistNumber FROM CTE_ALLOCATED)
			)

			SELECT T0.*
			FROM (
				SELECT WhsCode, DistNumber, AbsEntry, AssignedQty
				FROM CTE_AVAILABLE
				UNION
				SELECT WhsCode, DistNumber, AbsEntry, OnHandQty AS AssignedQty
				FROM CTE_ALLOCATED 
				WHERE AbsEntry NOT IN (SELECT AbsEntry FROM CTE_AVAILABLE)
			) T0
			ORDER BY T0.AbsEntry 
		END
		ELSE
		BEGIN
			SELECT T0.AbsEntry, T0.ItemCode, T1.DistNumber
			FROM OSRQ T0
			LEFT JOIN OSRN T1 ON T1.ItemCode = T0.ItemCode AND T0.MdAbsEntry = T1.AbsEntry 
			WHERE T0.ItemCode = JSON_VALUE(@Json, '$.itemCode') AND T0.WhsCode = JSON_VALUE(@Json, '$.whsCode') AND (T0.Quantity > 0 OR T0.CommitQty > 0 OR T0.CountQty > 0)
			ORDER BY T1.SysNumber
		END
	END

	-- ITEM WAREHOUSE BIN BATCH / SERIAL
	IF @Type = 'ItemWarehouseBinBatch' AND @Json LIKE '%whsCode%' AND @Json LIKE '%itemCode%'
	BEGIN
		IF @Json LIKE '%batchNumber%'
		BEGIN
			SELECT T0.ItemCode, T0.WhsCode, T1.DistNumber, T1.CreateDate, T2.BinCode, T0.OnHandQty, T0.AbsEntry, T0.BinAbs 
			FROM OBBQ T0
			LEFT OUTER JOIN OBTN T1 ON T1.ItemCode = T0.ItemCode AND T1.AbsEntry = T0.SnBMDAbs
			LEFT OUTER JOIN OBIN T2 ON T2.AbsEntry = T0.BinAbs 
			WHERE T0.ItemCode = JSON_VALUE(@Json, '$.itemCode') AND T0.WhsCode = JSON_VALUE(@Json, '$.whsCode') AND T0.OnHandQty > 0 AND T1.DistNumber = JSON_VALUE(@Json, '$.batchNumber')
			ORDER BY InDate
		END
		ELSE
		BEGIN
			SELECT T0.WhsCode, T1.DistNumber, T2.BinCode, T0.OnHandQty, T0.AbsEntry, T0.BinAbs
			FROM OBBQ T0
			LEFT OUTER JOIN OBTN T1 ON T1.ItemCode = T0.ItemCode AND T1.AbsEntry = T0.SnBMDAbs
			LEFT OUTER JOIN OBIN T2 ON T2.AbsEntry = T0.BinAbs 
			WHERE T0.ItemCode = JSON_VALUE(@Json, '$.itemCode') AND T0.WhsCode = JSON_VALUE(@Json, '$.whsCode') AND T0.OnHandQty > 0
			ORDER BY T1.InDate
		END
	END

	IF @Type = 'ItemWarehouseBinSerial' AND @Json LIKE '%whsCode%' AND @Json LIKE '%itemCode%'
	BEGIN
		SELECT T2.DistNumber, T1.BinCode, T0.WhsCode, T0.OnHandQty, T0.BinAbs, T0.SnBMDAbs, T0.AbsEntry
		FROM OSBQ T0 
		LEFT OUTER JOIN OBIN T1 ON T1.AbsEntry = T0.BinAbs
		LEFT OUTER JOIN OSRN T2 ON T2.AbsEntry = T0.SnBMDAbs
		LEFT OUTER JOIN OSRQ T3 ON T3.ItemCode = T2.ItemCode AND T3.MdAbsEntry = T2.AbsEntry AND T3.WhsCode = T0.WhsCode
		WHERE T0.ItemCode = JSON_VALUE(@Json, '$.itemCode') AND T0.WhsCode = JSON_VALUE(@Json, '$.whsCode') AND T0.OnHandQty > 0
		ORDER BY T2.SysNumber
	END

	IF @Type = 'ItemWarehouseBin' AND @Json LIKE '%whsCode%' AND @Json LIKE '%itemCode%'
	BEGIN
		SELECT T1.BinCode , T0.AbsEntry, T0.ItemCode, T0.BinAbs, T0.OnHandQty, T0.WhsCode
		FROM OIBQ T0
		INNER JOIN OBIN T1 ON T1.AbsEntry = T0.BinAbs 
		WHERE T0.ItemCode = JSON_VALUE(@Json, '$.itemCode') AND T0.WhsCode = JSON_VALUE(@Json, '$.whsCode') AND T0.OnHandQty > 0
	END

	-- PICK LIST
	IF @Type = 'PickList' AND @Json LIKE '%status%' AND @json LIKE '%startDate%' AND @json LIKE '%endDate%' AND @Json LIKE '%whsCode%' AND @json LIKE '%picker%'
	BEGIN
		WITH CTE AS (
			SELECT T0.AbsEntry, MAX(T1.WhsCode) AS WhsCode, MAX(T2.DocNum) AS DocNum
			FROM PKL1 T0 WITH (NOLOCK)
			INNER JOIN RDR1 T1 WITH (NOLOCK) ON T1.DocEntry = T0.OrderEntry AND T1.LineNum = T0.OrderLine
			INNER JOIN ORDR T2 WITH (NOLOCK) ON T2.DocEntry = T1.DocEntry
			INNER JOIN OITM T3 WITH (NOLOCK) ON T3.ItemCode = T1.ItemCode
			WHERE T3.InvntItem = 'Y' AND T1.WhsCode = JSON_VALUE(@Json, '$.whsCode')
			GROUP BY T0.AbsEntry
		)

		SELECT T0.AbsEntry AS DocEntry, T0.AbsEntry AS DocNum, T1.PickDate AS DocDate, CASE T1.[Status] WHEN 'R' THEN 'Released' WHEN 'Y' THEN 'Picked' WHEN 'P' THEN 'Partially Picked' WHEN 'D' THEN 'Partially Delivered' ELSE 'Closed' END AS [Status], 
			T0.DocNum AS SODocNum, T1.[Name]
		FROM CTE T0
		INNER JOIN OPKL T1 ON T1.AbsEntry = T0.AbsEntry
		WHERE T0.WhsCode = JSON_VALUE(@Json, '$.whsCode') AND T1.[Status] = JSON_VALUE(@Json, '$.status') AND T1.PickDate >= JSON_VALUE(@Json, '$.startDate') AND T1.PickDate  <= JSON_VALUE(@Json, '$.endDate') AND
			T1.[Name] = JSON_VALUE(@Json, '$.picker')
	END

	IF @Type = 'PickListDetails' AND @Json LIKE '%absEntry%'
	BEGIN
		DROP TABLE IF EXISTS #PKL1Temp;

		CREATE TABLE #PKL1Temp (Id NVARCHAR(50), DocEntry INT, PickEntry INT, PickQtty NUMERIC(19,6), PickStatus NVARCHAR(30), RelQtty NUMERIC(19,6), PrevReleas NUMERIC(19,6), WhsCode NVARCHAR(8), ItemCode NVARCHAR(50), 
			Dscription NVARCHAR(200), NumPerMsr NUMERIC(19,6), Measurement NVARCHAR(20), SODocEntry INT, SOLineNum INT, WhsBinActivat NVARCHAR(10), ManagedBy NVARCHAR(25))

		IF ((SELECT COUNT(1) FROM PKL1 WHERE AbsEntry = CAST(JSON_VALUE(@Json, '$.absEntry') AS INT) AND BaseObject = 17) > 0)
		BEGIN
			INSERT INTO #PKL1Temp
			SELECT NEWID() AS Id, T0.AbsEntry, T1.PickEntry, (T1.PickQtty / T2.NumPerMsr * T2.NumPerMsr2) AS PickQtty, 
				CASE T1.PickStatus WHEN 'R' THEN 'Released' WHEN 'Y' THEN 'Picked' WHEN 'P' THEN 'Partially Picked' WHEN 'D' THEN 'Partially Delivered' ELSE 'Closed' END AS PickStatus, (T1.RelQtty / T2.NumPerMsr * T2.NumPerMsr2) AS RelQtty,
				(T1.PrevReleas / T2.NumPerMsr * T2.NumPerMsr2) AS PrevReleas, T2.WhsCode, T2.ItemCode, T2.Dscription, T2.NumPerMsr, ISNULL(T2.unitMsr, T2.UomCode) AS Measurement, 
				T2.DocEntry AS SODocEntry, T2.LineNum AS SOLineNum, T6.BinActivat AS WhsBinActivat, CASE WHEN T4.ManBtchNum = 'Y' THEN 'BATCH' WHEN T4.ManSerNum = 'Y' THEN 'SERIAL' ELSE 'NONE' END AS ManagedBy
			FROM OPKL T0
			INNER JOIN PKL1 T1 WITH (NOLOCK) ON T0.AbsEntry = T1.AbsEntry
			INNER JOIN RDR1 T2 WITH (NOLOCK) ON T2.DocEntry = T1.OrderEntry AND T1.OrderLine = T2.LineNum AND BaseObject = 17
			INNER JOIN OITM T4 WITH (NOLOCK) ON T4.ItemCode = T2.ItemCode
			INNER JOIN OITW T5 WITH (NOLOCK) ON T5.ItemCode = T2.ItemCode AND T5.WhsCode = T2.WhsCode
			INNER JOIN OWHS T6 WITH (NOLOCK) ON T6.WhsCode = T2.WhsCode
			WHERE T0.AbsEntry = CAST(JSON_VALUE(@Json, '$.absEntry') AS INT)
		END

		SELECT * FROM #PKL1Temp

		DROP TABLE #PKL1Temp
	END

	IF @Type = 'PickListDetailsBin' AND @Json LIKE '%absEntry%' AND @Json LIKE '%pickEntry%'
	BEGIN
		SELECT T0.AbsEntry AS DocEntry, T0.Pkl2LinNum, T0.PickEntry, T0.ItemCode, T2.ItemName AS Dscription, T0.SnBEntry, T0.RelQtty, T0.PickQtty, T1.WhsCode,
			T1.BinCode, T0.BinAbs, (T0.PickQtty + T0.RelQtty) AS Quantity,
			(
				SELECT CASE T0.ManagedBy WHEN '10000044' THEN MAX(T4.DistNumber) WHEN '10000045' THEN MAX(T5.DistNumber) END AS DistNumber
				FROM ITL1 T3 
				LEFT JOIN OBTN T4 ON T3.SysNumber =T4.SysNumber AND T3.ItemCode = T4.ItemCode
				LEFT JOIN OSRN T5 ON T3.SysNumber = T5.SysNumber AND T3.ItemCode = T5.ItemCode
				WHERE T3.MdAbsEntry = T0.SnBEntry
			) [DistNumber]
		FROM PKL2 T0
		INNER JOIN OBIN T1 WITH (NOLOCK) ON T0.BinAbs = T1.AbsEntry
		INNER JOIN OITM T2 WITH (NOLOCK) ON T0.ItemCode = T2.ItemCode
		WHERE T0.AbsEntry = JSON_VALUE(@json, '$.absEntry') AND T0.PickEntry = JSON_VALUE(@json, '$.pickEntry')
		ORDER BY T0.Pkl2LinNum
	END

	IF @Type = 'PickListLineAllocation' AND @Json LIKE '%absEntry%' AND @Json LIKE '%lineNum%'
	BEGIN
		SELECT T0.DocEntry AS PickEntry, T0.AllocateLn, T1.MdAbsEntry, CASE T0.ManagedBy WHEN 10000044 THEN T2.DistNumber WHEN 10000045 THEN T3.DistNumber END AS DistNumber, SUM(T1.PickedQty) AS PickedQty
		FROM OITL T0
		INNER JOIN ITL1 T1 ON T1.LogEntry = T0.LogEntry
		LEFT JOIN OBTN T2 ON T2.AbsEntry = T1.MdAbsEntry
		LEFT JOIN OSRN T3 ON T3.AbsEntry = T1.MdAbsEntry
		WHERE T0.AllocatEnt = JSON_VALUE(@Json, '$.absEntry') AND T0.DocType = 156 AND T0.AllocateLn = JSON_VALUE(@Json, '$.lineNum')
		GROUP BY T0.DocEntry, T1.MdAbsEntry, T0.ManagedBy, T2.DistNumber, T3.DistNumber, T0.AllocateLn
		HAVING SUM(T1.PickedQty) > 0
	END

	IF @Type = 'PickListSerialAllocation' AND @Json LIKE '%absEntry%' AND @Json LIKE '%lineNum%'
	BEGIN
		WITH BaseInfo AS
		(
			SELECT T0.ObjType, MAX(T1.LogEntry) AS [MaxLogEntry], T1.ItemCode, T1.DocLine AS LineNum, T2.SysNumber, T1.ManagedBy
			FROM RDR1 T0
			INNER JOIN OITL T1 ON T1.DocEntry = T0.DocEntry AND T1.DocType = T0.ObjType AND T1.DocLine = T0.LineNum
			INNER JOIN ITL1 T2 ON T2.LogEntry = T1.LogEntry
			WHERE T0.DocEntry = JSON_VALUE(@Json, '$.absEntry') AND T0.LineNum = JSON_VALUE(@Json, '$.lineNum')
			GROUP BY T0.ObjType, T1.ItemCode, T1.DocLine, T2.SysNumber, T1.ManagedBy, T1.DocEntry
		)

		SELECT T0.LineNum AS PickEntry, T0.LineNum AS AllocateLn, T0.SysNumber AS MdAbsEntry, CASE WHEN V0.OrderedQty > 0 THEN V0.OrderedQty WHEN V0.AllocQty > 0 THEN V0.AllocQty ELSE 
			CASE WHEN V0.Quantity < 0 THEN V0.Quantity * -1 ELSE V0.Quantity END END AS PickedQty, T1.IntrSerial AS DistNumber
		FROM BaseInfo T0
		CROSS APPLY (
			SELECT I0.OrderedQty, I0.AllocQty, I0.Quantity
			FROM ITL1 I0
			WHERE I0.LogEntry = T0.MaxLogEntry AND I0.ItemCode = T0.ItemCode AND I0.SysNumber = T0.SysNumber AND (I0.OrderedQty > 0 OR I0.AllocQty > 0 OR I0.Quantity <> 0)
		) V0
		LEFT JOIN OSRI T1 ON T1.SysSerial = T0.[SysNumber] AND T1.ItemCode = T0.ItemCode AND T0.ManagedBy = 10000045
	END

	-- PRODUCTION ORDER
	IF @Type = 'ProductionOrders' AND @Json LIKE '%startDate%' AND @Json LIKE '%endDate%'
	BEGIN
		SELECT DocEntry, DocNum, CreateDate AS DocDate, ItemCode, PlannedQty, Warehouse
		FROM OWOR
		WHERE [Status] = 'R' AND CreateDate BETWEEN JSON_VALUE(@Json, '$.startDate') AND JSON_VALUE(@Json, '$.endDate')
	END

	IF @Type = 'ProductionOrderLines' AND @Json LIKE '%selected%'
	BEGIN
		SELECT ISNULL(LOWER(T4.Id), LOWER(NEWID())) AS Id, T0.DocEntry, T0.DocEntry AS DocNum, T0.LineNum, T0.ItemCode, T0.ItemName AS Dscription, T1.StageId, T0.ItemType, CASE WHEN (T0.PlannedQty - T0.IssuedQty) > 0 THEN T0.PlannedQty - T0.IssuedQty ELSE 0 END AS OpenQty,
			ISNULL(T0.PoQuantity, 0) AS PoQuantity, T0.wareHouse AS WhsCode, T0.UomCode AS Measurement, '202'  AS ObjType,
			CASE WHEN T2.ManBtchNum = 'Y' THEN 'BATCH' WHEN T2.ManSerNum = 'Y' THEN 'SERIAL' ELSE 'NONE' END AS ManagedBy, ISNULL(T4.Quantity, 0) AS ReceiptQty, ISNULL(T4.Allocations, '[]') AS Allocations, 
			CAST(CASE WHEN ISNULL(T4.Id, '') = '' THEN 0 ELSE 1 END AS BIT) AS IsDraft, T5.BinActivat AS WhsBinActivat
		FROM WOR1 T0
		LEFT JOIN WOR4 T1 ON T1.DocEntry = T0.DocEntry AND T1.StageId = T0.StageId
		LEFT JOIN OITM T2 ON T2.ItemCode = T0.ItemCode
		LEFT JOIN ORSC T3 ON T3.VisResCode = T0.ItemCode
		LEFT JOIN (
			SELECT I0.Id, I0.Quantity, I0.ShowList, I0.Allocations, I0.WhsCode, I0.LineNum, I1.UserId, I2.BinActivat, I0.BaseEntry
			FROM IMAppDocumentDraftLine I0
			INNER JOIN IMAppDocumentDraft I1 ON I1.Id = I0.DocGuid AND I1.[Status] = 'O' AND I0.BaseType = 202
			INNER JOIN OWHS I2 ON I2.WhsCode = I0.WhsCode
			WHERE I0.BaseEntry IN (SELECT [value] FROM STRING_SPLIT(JSON_VALUE(@Json, '$.selected'), ',')) AND I0.LineStatus = 'O' AND I1.ObjType = '60'
		) T4 ON T4.LineNum = T0.LineNum AND T4.UserId = JSON_VALUE(@Json, '$.userId') AND T4.BaseEntry = T0.DocEntry
		LEFT JOIN OWHS T5 ON T5.WhsCode = T0.wareHouse
		WHERE T0.DocEntry = JSON_VALUE(@Json, '$.selected') AND T0.IssueType = 'M' AND T0.ItemType IN (4, 290)
	END

	IF @Type = 'ProductionReceiptLines' AND @Json LIKE '%selected%'
	BEGIN
		SELECT ISNULL(LOWER(T4.Id), LOWER(NEWID())) AS Id, T0.DocEntry, T0.LineNum, T0.ItemCode, T0.ItemName AS Dscription, T0.wareHouse AS WhsCode, T0.IssuedQty AS OpenQty, T0.ItemType, T0.UomCode AS Measurement, 
			CAST(0 AS BIT) AS IsFG, CAST(CASE WHEN ISNULL(T4.Id, '') = '' THEN 0 ELSE 1 END AS BIT) AS IsDraft, ISNULL(T4.Allocations, '[]') AS Allocations,
			CASE WHEN T2.ManBtchNum = 'Y' THEN 'BATCH' WHEN T2.ManSerNum = 'Y' THEN 'SERIAL' ELSE 'NONE' END AS ManagedBy, ISNULL(T4.Quantity, 0) AS ReceiptQty, T5.BinActivat AS WhsBinActivat, '202' AS ObjType, T4.ShowList
		FROM WOR1 T0
		LEFT JOIN WOR4 T1 ON T1.DocEntry = T0.DocEntry AND T1.StageId = T0.StageId
		LEFT JOIN OITM T2 ON T2.ItemCode = T0.ItemCode
		LEFT JOIN ORSC T3 ON T3.VisResCode = T0.ItemCode
		LEFT JOIN (
			SELECT I0.Id, I0.Quantity, I0.ShowList, I0.Allocations, I0.WhsCode, I0.LineNum, I1.UserId, I2.BinActivat, I0.BaseEntry
			FROM IMAppDocumentDraftLine I0
			INNER JOIN IMAppDocumentDraft I1 ON I1.Id = I0.DocGuid AND I1.[Status] = 'O'
			INNER JOIN OWHS I2 ON I2.WhsCode = I0.WhsCode
			WHERE I0.BaseEntry = JSON_VALUE(@Json, '$.selected') AND I0.LineStatus = 'O' AND I1.ObjType = '59'
		) T4 ON T4.LineNum = T0.LineNum AND T4.UserId = 1 AND T4.BaseEntry = T0.DocEntry
		LEFT JOIN OWHS T5 ON T5.WhsCode = T0.wareHouse
		WHERE T0.DocEntry = JSON_VALUE(@Json, '$.selected') AND T0.IssueType = 'M' AND T0.ItemType IN (4, 290) AND T0.IssuedQty > 0

		UNION ALL

		SELECT ISNULL(LOWER(T4.Id), LOWER(NEWID())) AS Id, T0.DocEntry, -1 AS LineNum, T0.ItemCode, T0.ProdName AS Dscription, T0.Warehouse AS WhsCode, T0.PlannedQty AS IssuedQty, 4 AS ItemType, 
			COALESCE(T0.Uom, T3.UomCode, 'Manual') AS Measurement, CAST(1 AS BIT) AS IsFG,  CAST(CASE WHEN ISNULL(T4.Id, '') = '' THEN 0 ELSE 1 END AS BIT) AS IsDraft, ISNULL(T4.Allocations, '[]') AS Allocations,
			CASE WHEN T1.ManBtchNum = 'Y' THEN 'BATCH' WHEN T1.ManSerNum = 'Y' THEN 'SERIAL' ELSE 'NONE' END AS ManagedBy, ISNULL(T4.Quantity, 0) AS ReceiptQty, T5.BinActivat AS WhsBinActivat, '202' AS ObjType, T4.ShowList
		FROM OWOR T0
		LEFT JOIN OITM T1 ON T1.ItemCode = T0.ItemCode
		LEFT JOIN OBCD T2 ON T2.ItemCode = T1.ItemCode
		LEFT JOIN OUOM T3 ON T3.UomEntry = T2.UomEntry
		LEFT JOIN (
			SELECT I0.Id, I0.Quantity, I0.ShowList, I0.Allocations, I0.WhsCode, I0.LineNum, I1.UserId, I2.BinActivat, I0.BaseEntry
			FROM IMAppDocumentDraftLine I0
			INNER JOIN IMAppDocumentDraft I1 ON I1.Id = I0.DocGuid AND I1.[Status] = 'O'
			INNER JOIN OWHS I2 ON I2.WhsCode = I0.WhsCode
			WHERE I0.BaseEntry = JSON_VALUE(@Json, '$.selected') AND I0.LineStatus = 'O' AND I1.ObjType = '59'
		) T4 ON T4.LineNum = -1 AND T4.UserId = 1 AND T4.BaseEntry = T0.DocEntry
		LEFT JOIN OWHS T5 ON T5.WhsCode = T0.wareHouse
		WHERE T0.DocEntry = JSON_VALUE(@Json, '$.selected')
	END

	IF @Type = 'ProductionOrderStages' AND @Json LIKE '%docEntry%'
	BEGIN
		SELECT StageId, SeqNum, [Name], StgEntry
		FROM WOR4
		WHERE DocEntry = JSON_VALUE(@Json, '$.docEntry')
		ORDER BY SeqNum
	END

	-- PURCHASE ORDER
	IF @Type = 'PurchaseOrders' AND @Json LIKE '%startDate%' AND @Json LIKE '%endDate%'
	BEGIN
		SELECT DocEntry, DocNum, DocDate, CardCode, CardName
		FROM OPOR
		WHERE DocStatus = 'O' AND DocDate >= JSON_VALUE(@Json, '$.startDate') AND DocDate <= JSON_VALUE(@Json, '$.endDate') AND DocType = 'I'
	END

	IF @Type = 'PurchaseOrderLines' AND @Json LIKE '%selected%'
	BEGIN
		SELECT ISNULL(LOWER(T4.Id), LOWER(NEWID())) AS Id, T0.DocEntry, T1.DocNum, T0.LineNum, T0.ItemCode, T0.Dscription, ISNULL(T0.unitMsr, T0.UomCode) AS Measurement, T0.NumPerMsr, 
			T0.ObjType, ISNULL(T4.WhsCode, T0.WhsCode) AS WhsCode, ISNULL(T4.BinActivat, T3.BinActivat) AS WhsBinActivat, T0.OpenQty, T1.CardCode, T1.CardName, ISNULL(T4.Quantity, 0) AS ReceiptQty, 
			T4.ShowList, ISNULL(T4.Allocations, '[]') AS Allocations, CASE WHEN T2.ManBtchNum = 'Y' THEN 'BATCH' WHEN T2.ManSerNum = 'Y' THEN 'SERIAL' ELSE 'NONE' END AS ManagedBy, CAST(CASE WHEN ISNULL(T4.Id, '') = '' THEN 0 ELSE 1 END AS BIT) AS IsDraft
		FROM POR1 T0
		INNER JOIN OPOR T1 ON T1.DocEntry = T0.DocEntry
		INNER JOIN OITM T2 ON T2.ItemCode = T0.ItemCode
		INNER JOIN OWHS T3 ON T3.WhsCode = T0.WhsCode
		LEFT JOIN (
			SELECT I0.Id, I0.Quantity, I0.ShowList, I0.Allocations, I0.WhsCode, I0.LineNum, I1.UserId, I2.BinActivat, I0.BaseEntry
			FROM IMAppDocumentDraftLine I0
			INNER JOIN IMAppDocumentDraft I1 ON I1.Id = I0.DocGuid AND I1.[Status] = 'O' AND I1.ObjType = 20
			INNER JOIN OWHS I2 ON I2.WhsCode = I0.WhsCode
			WHERE I0.BaseEntry IN (SELECT [value] FROM STRING_SPLIT(JSON_VALUE(@Json, '$.selected'), ',')) AND I0.LineStatus = 'O'
		) T4 ON T4.LineNum = T0.LineNum AND T4.UserId = JSON_VALUE(@Json, '$.userId') AND T4.BaseEntry = T0.DocEntry
		WHERE T0.DocEntry IN (SELECT [value] FROM STRING_SPLIT(JSON_VALUE(@Json, '$.selected'), ',')) AND T0.LineStatus = 'O'
		ORDER BY T0.DocEntry, T0.LineNum
	END

	-- RESOURCES
	IF @Type = 'Resources'
	BEGIN
		SELECT VisResCode, ResName, CASE ResType WHEN 'M' THEN 'Machine' WHEN 'L' THEN 'Labor' ELSE 'Other' END AS ResType
		FROM ORSC
		WHERE frozenFor = 'N'
	END

	-- RETURN REQUEST
	IF @Type = 'ReturnRequests' AND @Json LIKE '%startDate%' AND @Json LIKE '%endDate%'
	BEGIN
		SELECT DocEntry, DocNum, DocDate, CardCode, CardName
		FROM ORRR
		WHERE DocStatus = 'O' AND DocDate >= JSON_VALUE(@Json, '$.startDate') AND DocDate <= JSON_VALUE(@Json, '$.endDate') AND DocType = 'I'
			AND DocEntry NOT IN (SELECT I0.DocEntry FROM RRR1 I0 WHERE I0.BaseType = 13 GROUP BY I0.DocEntry)
	END

	IF @Type = 'ReturnRequestLines' AND @Json LIKE '%selected%'
	BEGIN
		SELECT ISNULL(LOWER(T4.Id), LOWER(NEWID())) AS Id, T0.DocEntry, T1.DocNum, T0.LineNum, T0.ItemCode, T0.Dscription, ISNULL(T0.unitMsr, T0.UomCode) AS Measurement, T0.NumPerMsr, T0.ObjType, 
			ISNULL(T4.WhsCode, T0.WhsCode) AS WhsCode, ISNULL(T4.BinActivat, T3.BinActivat) AS WhsBinActivat, T0.OpenCreQty AS OpenQty, T1.CardCode, T1.CardName, ISNULL(T4.Quantity, 0) AS ReceiptQty,
			T4.ShowList, ISNULL(T4.Allocations, '[]') AS Allocations, CASE WHEN T2.ManBtchNum = 'Y' THEN 'BATCH' WHEN T2.ManSerNum = 'Y' THEN 'SERIAL' ELSE 'NONE' END AS ManagedBy, CAST(CASE WHEN ISNULL(T4.Id, '') = '' THEN 0 ELSE 1 END AS BIT) AS IsDraft
		FROM RRR1 T0
		INNER JOIN ORRR T1 ON T1.DocEntry = T0.DocEntry
		INNER JOIN OITM T2 ON T2.ItemCode = T0.ItemCode
		INNER JOIN OWHS T3 ON T3.WhsCode = T0.WhsCode
		LEFT JOIN (
			SELECT I0.Id, I0.Quantity, I0.ShowList, I0.Allocations, I0.WhsCode, I0.LineNum, I1.UserId, I2.BinActivat, I0.BaseEntry
			FROM IMAppDocumentDraftLine I0
			INNER JOIN IMAppDocumentDraft I1 ON I1.Id = I0.DocGuid AND I1.[Status] = 'O' AND I0.BaseType = JSON_VALUE(@Json, '$.objType')
			INNER JOIN OWHS I2 ON I2.WhsCode = I0.WhsCode
			WHERE I0.BaseEntry IN (SELECT [value] FROM STRING_SPLIT(JSON_VALUE(@Json, '$.selected'), ',')) AND I0.LineStatus = 'O'
		) T4 ON T4.LineNum = T0.LineNum AND T4.UserId = JSON_VALUE(@Json, '$.userId') AND T4.BaseEntry = T0.DocEntry
		WHERE T0.DocEntry IN (SELECT [value] FROM STRING_SPLIT(JSON_VALUE(@Json, '$.selected'), ',')) AND T0.LineStatus = 'O'
		ORDER BY T0.DocEntry, T0.LineNum
	END

	-- SALES ORDER
	IF @Type = 'SalesOrders' AND @Json LIKE '%startDate%' AND @Json LIKE '%endDate%'
	BEGIN
		SELECT DocEntry, DocNum, DocDate, CardCode, CardName
		FROM ORDR
		WHERE DocStatus = 'O' AND DocDate >= JSON_VALUE(@Json, '$.startDate') AND DocDate <= JSON_VALUE(@Json, '$.endDate') AND DocType = 'I'
	END

	IF @Type = 'SalesOrderLines' AND @Json LIKE '%selected%'
	BEGIN
		SELECT ISNULL(LOWER(T4.Id), LOWER(NEWID())) AS Id, T0.DocEntry, T1.DocNum, T0.LineNum, T0.ItemCode, T0.Dscription, ISNULL(T0.unitMsr, T0.UomCode) AS Measurement, T0.NumPerMsr, T0.ObjType, 
			ISNULL(T4.WhsCode, T0.WhsCode) AS WhsCode, ISNULL(T4.BinActivat, T3.BinActivat) AS WhsBinActivat, T0.OpenQty, T1.CardCode, T1.CardName, ISNULL(T4.Quantity, 0) AS ReceiptQty,
			T4.ShowList, ISNULL(T4.Allocations, '[]') AS Allocations, CASE WHEN T2.ManBtchNum = 'Y' THEN 'BATCH' WHEN T2.ManSerNum = 'Y' THEN 'SERIAL' ELSE 'NONE' END AS ManagedBy, CAST(CASE WHEN ISNULL(T4.Id, '') = '' THEN 0 ELSE 1 END AS BIT) AS IsDraft
		FROM RDR1 T0
		INNER JOIN ORDR T1 ON T1.DocEntry = T0.DocEntry
		INNER JOIN OITM T2 ON T2.ItemCode = T0.ItemCode
		INNER JOIN OWHS T3 ON T3.WhsCode = T0.WhsCode
		LEFT JOIN (
			SELECT I0.Id, I0.Quantity, I0.ShowList, I0.Allocations, I0.WhsCode, I0.LineNum, I1.UserId, I2.BinActivat, I0.BaseEntry
			FROM IMAppDocumentDraftLine I0
			INNER JOIN IMAppDocumentDraft I1 ON I1.Id = I0.DocGuid AND I1.[Status] = 'O'
			INNER JOIN OWHS I2 ON I2.WhsCode = I0.WhsCode
			WHERE I0.BaseEntry IN (SELECT [value] FROM STRING_SPLIT(JSON_VALUE(@Json, '$.selected'), ',')) AND I0.LineStatus = 'O'
		) T4 ON T4.LineNum = T0.LineNum AND T4.UserId = JSON_VALUE(@Json, '$.userId') AND T4.BaseEntry = T0.DocEntry
		WHERE T0.DocEntry IN (SELECT [value] FROM STRING_SPLIT(JSON_VALUE(@Json, '$.selected'), ',')) AND T0.LineStatus = 'O'
		ORDER BY T0.DocEntry, T0.LineNum
	END

	-- SERIES
	IF @Type = 'DocSeries'
	BEGIN
		SELECT T0.ObjectCode, T0.Series, T0.SeriesName, CAST(CASE WHEN T1.DfltSeries <> T0.Series THEN 0 ELSE 1 END AS BIT) IsDefault
		FROM NNM1 T0
		INNER JOIN ONNM T1 ON T1.ObjectCode = T0.ObjectCode
		WHERE T0.ObjectCode = JSON_VALUE(@Json, '$.objectCode')
	END

	-- SERIAL NUMBER
	IF @Type = 'SerialNumberInfo'
	BEGIN
		SELECT ItemCode, DistNumber
		FROM OSRN
		WHERE DistNumber = JSON_VALUE(@Json, '$.serial')
	END

	-- STOCK TRANSFER
	IF @Type = 'StockTransferRequest' AND @Json LIKE '%startDate%' AND @Json LIKE '%endDate%'
	BEGIN
		SELECT T0.DocEntry, T0.DocNum, T0.DocDate, T0.Filler AS FromWhsCode, T0.ToWhsCode AS WhsCode, (SELECT TOP 1 WhsCode FROM OWHS WHERE U_IsGIT = 'Y') AS GITWhsCode
		FROM OWTQ T0
		WHERE DocStatus = 'O' AND DocDate >= JSON_VALUE(@Json, '$.startDate') AND DocDate <= JSON_VALUE(@Json, '$.endDate') AND DocType = 'I'
	END

	IF @Type = 'StockTransferRequestLines' AND @Json LIKE '%selected%'
	BEGIN
		SELECT ISNULL(LOWER(T4.Id), LOWER(NEWID())) AS Id, T0.DocEntry, T0.LineNum, T1.DocNum, T0.ItemCode, T0.Dscription, ISNULL(T0.unitMsr, T0.UomCode) AS Measurement, T0.ObjType,
			ISNULL(T4.WhsCode, T0.WhsCode) AS WhsCode, ISNULL(T4.BinActivat, T3.BinActivat) AS WhsBinActivat, T0.Quantity AS OpenQty, T0.FromWhsCod AS FromWhsCode, ISNULL(T4.Quantity, 0) AS ReceiptQty,
			T4.ShowList, ISNULL(T4.Allocations, '[]') AS Allocations, CASE WHEN T2.ManBtchNum = 'Y' THEN 'BATCH' WHEN T2.ManSerNum = 'Y' THEN 'SERIAL' ELSE 'NONE' END AS ManagedBy, CAST(CASE WHEN ISNULL(T4.Id, '') = '' THEN 0 ELSE 1 END AS BIT) AS IsDraft
		FROM WTQ1 T0
		INNER JOIN OWTQ T1 ON T1.DocEntry = T0.DocEntry
		INNER JOIN OITM T2 ON T2.ItemCode = T0.ItemCode
		INNER JOIN OWHS T3 ON T3.WhsCode = T0.FromWhsCod
		LEFT JOIN (
			SELECT I0.Id, I0.Quantity, I0.ShowList, I0.Allocations, I0.WhsCode, I0.LineNum, I1.UserId, I2.BinActivat, I0.BaseEntry
			FROM IMAppDocumentDraftLine I0
			INNER JOIN IMAppDocumentDraft I1 ON I1.Id = I0.DocGuid AND I1.[Status] = 'O'
			INNER JOIN OWHS I2 ON I2.WhsCode = I0.FromWhsCode
			WHERE I0.BaseEntry IN (SELECT [value] FROM STRING_SPLIT(JSON_VALUE(@Json, '$.selected'), ',')) AND I0.LineStatus = 'O'
		) T4 ON T4.LineNum = T0.LineNum AND T4.UserId = JSON_VALUE(@Json, '$.userId') AND T4.BaseEntry = T0.DocEntry
		WHERE T0.DocEntry IN (SELECT [value] FROM STRING_SPLIT(JSON_VALUE(@Json, '$.selected'), ',')) AND T0.LineStatus = 'O'
		ORDER BY T0.DocEntry, T0.LineNum
	END

	IF @Type = 'PutStockTransfer' AND @Json LIKE '%startDate%' AND @Json LIKE '%endDate%'
	BEGIN
		SELECT T0.DocEntry, T0.DocNum, T0.DocDate, T0.Filler AS FromWhsCode, T0.ToWhsCode AS WhsCode, (SELECT TOP 1 WhsCode FROM OWHS WHERE U_IsGIT = 'Y') AS GITWhsCode, ISNULL(CAST(T1.DocNum AS NVARCHAR), '-') AS BaseDocNum
		FROM OWTR T0
		LEFT JOIN (
			SELECT I1.DocEntry, I1.DocNum
			FROM WTQ1 I0
			INNER JOIN OWTQ I1 ON I1.DocEntry = I0.BaseEntry
			GROUP BY I1.DocNum, I1.DocEntry
		) T1 ON T1.DocEntry = T0.DocEntry
		WHERE T0.DocStatus = 'O' AND T0.DocDate >= JSON_VALUE(@Json, '$.startDate') AND T0.DocDate <= JSON_VALUE(@Json, '$.endDate') AND T0.DocType = 'I' AND T0.U_Direction = 'FROM'
			AND T0.DocEntry NOT IN (SELECT U_RefNumber FROM OWTR WHERE U_Direction = 'TO' AND U_RefNumber IS NOT NULL)
	END

	IF @Type = 'PutStockTransferLines'
	BEGIN
		SELECT ISNULL(LOWER(T4.Id), LOWER(NEWID())) AS Id, T0.DocEntry, T0.LineNum, T1.DocNum, T0.ItemCode, T0.Dscription, ISNULL(T0.unitMsr, T0.UomCode) AS Measurement, T0.ObjType,
			ISNULL(T4.WhsCode, T1.ToWhsCode) AS WhsCode, ISNULL(T4.BinActivat, T3.BinActivat) AS WhsBinActivat, T0.Quantity AS OpenQty, T0.FromWhsCod AS FromWhsCode, ISNULL(T4.Quantity, 0) AS ReceiptQty, T0.Quantity,
			T4.ShowList, ISNULL(T4.Allocations, '[]') AS Allocations, CASE WHEN T2.ManBtchNum = 'Y' THEN 'BATCH' WHEN T2.ManSerNum = 'Y' THEN 'SERIAL' ELSE 'NONE' END AS ManagedBy, CAST(CASE WHEN ISNULL(T4.Id, '') = '' THEN 0 ELSE 1 END AS BIT) AS IsDraft
		FROM WTR1 T0
		INNER JOIN OWTR T1 ON T1.DocEntry = T0.DocEntry
		INNER JOIN OITM T2 ON T2.ItemCode = T0.ItemCode
		INNER JOIN OWHS T3 ON T3.WhsCode = T1.ToWhsCode
		LEFT JOIN (
			SELECT I0.Id, I0.Quantity, I0.ShowList, I0.Allocations, I0.WhsCode, I0.LineNum, I1.UserId, I2.BinActivat, I0.BaseEntry
			FROM IMAppDocumentDraftLine I0
			INNER JOIN IMAppDocumentDraft I1 ON I1.Id = I0.DocGuid AND I1.[Status] = 'O'
			INNER JOIN OWHS I2 ON I2.WhsCode = I0.WhsCode
			WHERE I0.BaseEntry IN (SELECT [value] FROM STRING_SPLIT(JSON_VALUE(@Json, '$.selected'), ',')) AND I0.LineStatus = 'O'
		) T4 ON T4.LineNum = T0.LineNum AND T4.UserId = JSON_VALUE(@Json, '$.userId') AND T4.BaseEntry = T0.DocEntry
		WHERE T0.DocEntry = JSON_VALUE(@Json, '$.selected') AND T0.LineStatus = 'O'
		ORDER BY T0.DocEntry, T0.LineNum
	END

	IF @Type = 'PutStockTransferBatch' AND @Json LIKE '%docEntry%' AND @Json LIKE '%lineNum%'
	BEGIN
		SELECT T1.Quantity, T1.BatchNum AS DistNumber
		FROM WTR1 T0
		INNER JOIN IBT1 T1 ON T1.BaseEntry = T0.DocEntry AND T1.BaseLinNum = T0.LineNum AND T1.BaseType = T0.ObjType 
		INNER JOIN OWTR T2 ON T2.DocEntry = T0.DocEntry AND T1.ItemCode = T0.ItemCode
		WHERE T0.DocEntry = JSON_VALUE(@Json, '$.docEntry') AND T0.LineNum = JSON_VALUE(@Json, '$.lineNum') AND T1.Direction = 0
	END

	IF @Type = 'PutStockTransferSerial' AND @Json LIKE '%docEntry%' AND @Json LIKE '%lineNum%'
	BEGIN
		SELECT T1.ItemCode, T2.IntrSerial AS DistNumber
		FROM SRI1 T0
		INNER JOIN WTR1 T1 ON T0.BaseEntry = T1.DocEntry AND T0.BaseType = T1.ObjType AND T0.BaseLinNum = T1.LineNum
		INNER JOIN OSRI T2 ON T0.SysSerial = T2.SysSerial AND T0.ItemCode = T2.ItemCode
		WHERE T0.Direction = 0 AND T1.DocEntry = JSON_VALUE(@Json, '$.docEntry') AND T1.LineNum = JSON_VALUE(@Json, '$.lineNum')
	END

	-- WAREHOUSES
	IF @Type = 'Warehouses'
	BEGIN
		IF @Json LIKE '%isGIT%'
		BEGIN
			SELECT WhsCode, WhsName, BinActivat, U_IsGIT
			FROM OWHS
			WHERE U_IsGIT = 'Y'
		END
		ELSE
		BEGIN
			SELECT WhsCode, WhsName, BinActivat, U_IsGIT
			FROM OWHS 
			WHERE U_IsGIT = 'N'
		END
	END

	-- WAREHOUSE BIN
	IF @Type = 'WarehouseBins'
	BEGIN
		SET @query = 'SELECT T0.AbsEntry, T0.BinCode, T0.WhsCode FROM OBIN T0 WHERE T0.WhsCode = ''' + JSON_VALUE(@Json, '$.whsCode') + ''' '

		IF @Json LIKE '%binCode%'
		BEGIN
			SET @query += 'AND T0.BinCode LIKE ''%' + JSON_VALUE(@Json, '$.binCode') + '%'' '
		END

		EXEC (@query)
	END

	-- WAREHOUSE ITEMS
	IF @Type = 'WarehouseItems' AND @Json LIKE '%whsCode%'
	BEGIN
		SELECT T0.WhsCode, T2.WhsName, T0.ItemCode, T1.ItemName, T0.OnHand
		FROM OITW T0
		INNER JOIN OITM T1 ON T1.ItemCode = T0.ItemCode
		INNER JOIN OWHS T2 ON T2.WhsCode = T0.WhsCode
		WHERE T0.OnHand > 0 AND T0.WhsCode = JSON_VALUE(@Json, '$.whsCode')
	END

	IF @Type = 'DefaultWarehouse'
	BEGIN
		SELECT T1.WhsName, T0.DfltWhs AS WhsCode
		FROM OADM T0
		INNER JOIN OWHS T1 ON T1.WhsCode = T0.DfltWhs 
	END
END
GO


