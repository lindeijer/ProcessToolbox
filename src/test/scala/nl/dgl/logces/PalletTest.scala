package nl.dgl.logces

class PalletSetup {

  val product1 = Product("P1")
  val product2 = Product("P2")

  val article_P1_10 = Article("A-P1-10", product1, 10.0); // 10 kg of P1 in article-bag
  val article_P1_20 = Article("A-P1-20", product1, 20.0);
  val article_P1_30 = Article("A-P1-30", product1, 30.0);
  val article_P1_40 = Article("A-P1-40", product1, 40.0);

  val article_P2_30 = Article("A-P2-30", product2, 30.0); // 30 kg of P2 in article-bag
  val article_P2_40 = Article("A-P2-40", product2, 40.0);
  val article_P2_50 = Article("A-P2-50", product2, 50.0);
  val article_P2_60 = Article("A-P2-60", product2, 60.0);

  Pallet("WarehousePallet1", article_P1_10, 100) // 100 article-bags on the warehouse pallet
  Pallet("WarehousePallet2", article_P1_20, 100)
  Pallet("WarehousePallet3", article_P1_30, 100)
  Pallet("WarehousePallet4", article_P1_40, 100)
  Pallet("WarehousePallet5", article_P2_30, 200)
  Pallet("WarehousePallet6", article_P2_40, 200)
  Pallet("WarehousePallet7", article_P2_50, 200)
  Pallet("WarehousePallet8", article_P2_60, 200)

}

