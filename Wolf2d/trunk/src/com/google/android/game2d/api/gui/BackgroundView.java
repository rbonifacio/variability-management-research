package com.google.android.game2d.api.gui;

import java.util.List;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.drawable.BitmapDrawable;
import android.view.KeyEvent;
import android.widget.ImageView;

import com.google.android.game2d.api.AbstractSprite;
import com.google.android.game2d.wolf.sprites.wolverine.WolverineSprite;

/**
 * Basic background for game scenes. It class does not 
 * need to be specialized, just instatiated with a background 
 * image. Then, this kind of view is responsible for rendering 
 * the background
 *   
 * @author rbonifacio
 */
public final class BackgroundView extends ImageView {
	
	private Bitmap background; 
	private List<AbstractSprite> sprites;
	
	/**
	 * Create a new instance of BackgroundView
	 * @param context the application context
	 * @param background the background image used to render a game scene
	 */
	public BackgroundView(Context context, Bitmap background) {
		super(context);
		this.background = background;	
		defineBackground();
	}
	
	private void defineBackground() {
		setBackground(new BitmapDrawable(background));
	}
	
	public void setSprites(List<AbstractSprite> sprites) {
		this.sprites = sprites;
	}
	
	@Override
	protected void onDraw(Canvas canvas) {
		if(sprites != null) {
			for (AbstractSprite sprite : sprites) {
				canvas.drawBitmap(sprite.getBitmap(), sprite.getLeft(), sprite.getTop(), new Paint());
			}
		}
		AbstractSprite mc = WolverineSprite.instance();
		canvas.drawBitmap(mc.getBitmap(), mc.getLeft(), mc.getTop(), new Paint());
		super.onDraw(canvas);
	}

	@Override
	public boolean onKeyDown(int keyCode, KeyEvent event) {
		switch(keyCode) {
			case KeyEvent.KEYCODE_DPAD_LEFT : WolverineSprite.instance().moveLeft(); break;
			case KeyEvent.KEYCODE_DPAD_RIGHT : WolverineSprite.instance().moveRight(); break;
			case KeyEvent.KEYCODE_DPAD_UP : WolverineSprite.instance().moveUp(); break;
			case KeyEvent.KEYCODE_DPAD_DOWN : WolverineSprite.instance().moveDown(); break;
		}
		postInvalidate();
		return true;
	}

	
}
