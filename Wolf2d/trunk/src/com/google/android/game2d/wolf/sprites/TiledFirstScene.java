package com.google.android.game2d.wolf.sprites;

import android.content.Context;
import android.graphics.Bitmap;

import com.google.android.game2d.api.gui.TiledView;

public class TiledFirstScene extends TiledView {
	
	public TiledFirstScene(Context context, Bitmap bitmap) {
		super(context, bitmap);
	}

	protected void defineTiles() {
		int topRows = rows * 20 / 100;
		int midRows = rows * 30 / 100;
		int botRows = rows - (topRows + midRows);
		
		for(int i = 0; i < topRows; i ++) {
			for(int j = 0; j < columns; j++) {
				map[j][i] = 3;
			}
		}
		
		for(int i = topRows; i < (topRows + midRows); i++) {
			for(int j = 0; j < columns; j++) {
				map[j][i] = 4;
			}
		}
		
		for(int j = 0; j < columns; j++) {
			map[j][topRows+midRows] = 3;
		}
		
		for(int i = (topRows + midRows + 1); i < (topRows + midRows + botRows); i++) {
			for(int j = 0; j < columns; j++) {
				map[j][i] = 2;
			}
		}
	}
}
