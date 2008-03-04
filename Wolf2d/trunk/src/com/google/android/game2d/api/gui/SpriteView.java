package com.google.android.game2d.api.gui;

import android.content.Context;
import android.view.KeyEvent;
import android.widget.ImageView;

import com.google.android.game2d.api.sprite.AbstractSprite;

public class SpriteView extends ImageView {

	protected AbstractSprite sprite;
	
	public SpriteView(Context context, AbstractSprite sprite) {
		super(context);
		this.sprite = sprite;
		setImageBitmap(this.sprite.getBitmap());
		setPreferredWidth(this.sprite.getWidth());
        setPreferredHeight(this.sprite.getHeight());
        setAdjustViewBounds(false);
        setPadding(sprite.getLeft(), sprite.getTop(), 10+this.sprite.getWidth(), 10+this.sprite.getHeight());
        setFocusable(true);
	}

	@Override
	public boolean onKeyDown(int keyCode, KeyEvent event) {
		switch(keyCode) {
			case KeyEvent.KEYCODE_DPAD_LEFT : sprite.moveLeft();break;
			case KeyEvent.KEYCODE_DPAD_RIGHT : sprite.moveRight(); break;
			case KeyEvent.KEYCODE_DPAD_UP : sprite.moveUp(); break;
			case KeyEvent.KEYCODE_DPAD_DOWN : sprite.moveDown();
		}
		setImageBitmap(this.sprite.getBitmap());
		setPadding(sprite.getLeft(), sprite.getTop(), 10+this.sprite.getWidth(), 10+this.sprite.getHeight());
		return true;
	}
	
	
}

