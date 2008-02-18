package com.google.android.game2d.wolf.sprites.wolverine;

public class RightState extends AbstractWolverineState {

	@Override
	public void moveDown(WolverineSprite sprite) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void moveLeft(WolverineSprite sprite) {
		int column = 8;
		int row = 2;
		sprite.setFrame(column, row);
		sprite.setState(AbstractWolverineState.getLeftState());
	}

	@Override
	public void moveRight(WolverineSprite sprite) {
		int column = sprite.getCurrentColumn() == 5 ? 0 : sprite.getCurrentColumn() + 1;  
		int row = 0;
		sprite.setFrame(column, row);
		sprite.setLeft(sprite.getLeft()+5);
	}

	@Override
	public void moverUp(WolverineSprite sprite) {
		// TODO Auto-generated method stub
	}

	
}
